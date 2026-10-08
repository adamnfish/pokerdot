package io.adamnfish.pokerdot

import cats.*
import cats.effect.kernel.{Clock, Sync, Async}
import cats.implicits.*
import cats.syntax.all.*
import io.adamnfish.pokerdot.logic.{Games, PlayerActions, Representations, Responses}
import io.adamnfish.pokerdot.models.*
import io.adamnfish.pokerdot.services.{Database, Messaging}
import io.adamnfish.pokerdot.validation.Validation.*
import io.circe.Json


object PokerDot {
  def pokerdot[F[_] : MonadThrow](requestBody: String, appContext: AppContext[F]): F[String] = {
    (for {
      requestJson <- Serialisation.parse(requestBody, "could not understand the request", None)
      operationJson <- MonadThrow[F].fromOption(
        requestJson.hcursor.downField("operation").focus,
        Failures("Request did not include operation field", "could not understand the request")
      )
      operation <- Serialisation.extractJson[F, String](operationJson, "unexpected operation")
      response: Response[Message] <- operation match {
        case "create-game" =>
          appContext.rng.randomState.flatMap { initialSeed =>
            createGame(requestJson, appContext, initialSeed).widen[Response[Message]]
          }
        case "join-game" =>
          joinGame(requestJson, appContext).widen[Response[Message]]
        case "start-game" =>
          startGame(requestJson, appContext).widen[Response[Message]]
        case "bet" =>
          bet(requestJson, appContext).widen[Response[Message]]
        case "check" =>
          check(requestJson, appContext).widen[Response[Message]]
        case "fold" =>
          fold(requestJson, appContext).widen[Response[Message]]
        case "advance-phase" =>
          advancePhase(requestJson, appContext)
        case "update-blind" =>
          updateBlind(requestJson, appContext).widen[Response[Message]]
        // TODO: include admin endpoint to allow manual correction of game state
        case "ping" =>
          ping(requestJson, appContext).widen[Response[Message]]
        case "wake" =>
          wake(appContext).widen[Response[Message]]
        case _ =>
          MonadThrow[F].raiseError[Response[Message]] {
            Failures(
              s"Unexpected operation: $operation",
              "the request wasn't something I understand"
            )
          }
      }
      _ <- sendResponse(response, appContext.messaging)
    } yield operation)
      .onError {
        case failures: Failures =>
          // There are some failure messages that we don't want to send to clients (e.g. failed message delivery).
          // It's not urgent, but prevents cluttering a user's experience with irrelevant failure information.
          failures.externalFailures match {
            case Nil =>
              // if all the messages were 'internal' then there's no need to send a failure message
              MonadThrow[F].unit
            case externalFailures =>
              appContext.messaging.sendError(appContext.playerAddress, failures.externalOnly)
          }
      }
  }

  /**
   * Attempts every send, so one failed connection doesn't stop the others receiving their messages.
   */
  def sendResponse[F[_] : MonadThrow](response: Response[Message], messaging: Messaging[F]): F[Unit] = {
    val allMessages = response.messages.toList ++ response.statuses.toList
    for {
      results <- allMessages.traverse { case (address, msg: Message) =>
        messaging.sendMessage(address, msg).attempt
      }
      _ <- results.collect { case Left(e) => e } match {
        case Nil =>
          MonadThrow[F].unit
        case errors =>
          MonadThrow[F].raiseError(Failures(errors.flatMap {
            case failures: Failures =>
              failures.failures
            case e =>
              List(Failure(s"Unexpected error sending message: ${e.getMessage}", "unable to send message", exception = Some(e), internal = true))
          }))
      }
    } yield ()
  }

  // OPERATIONS

  def createGame[F[_] : MonadThrow](requestJson: Json, appContext: AppContext[F], initialSeed: Long): F[Response[Welcome]] = {
    for {
      createGame <- extractCreateGame(requestJson)
      now <- appContext.time.now
      rawGame = Games.newGame(createGame.gameName, trackStacks = false, now, initialSeed)
      uniqueGameCode <- Games.makeUniquePrefix(rawGame.gameId, appContext.db, Database.checkUniquePrefix)
      game = rawGame.copy(gameCode = uniqueGameCode)
      host = Games.newPlayer(game.gameId, createGame.screenName, isHost = true, now)
      gameWithHost = Games.addPlayer(game, host)
      gameDb = Representations.gameToDb(gameWithHost)
      hostDb = Representations.playerToDb(host)
      response = Responses.welcome(gameWithHost, host, appContext.playerAddress, Map(host.playerId -> Set(appContext.playerAddress)))
      _ <- appContext.db.writeGame(gameDb)
      _ <- appContext.db.writePlayer(hostDb)
      _ <- appContext.db.putConnection(connectionDb(gameWithHost, host.playerId, appContext.playerAddress))
    } yield response
  }

  /**
   * Allows a player to join a pending game.
   *
   * TODO: this or another operation should allow spectators
   */
  def joinGame[F[_] : MonadThrow](requestJson: Json, appContext: AppContext[F]): F[Response[Welcome]] = {
    for {
      rawJoinGame <- extractJoinGame(requestJson)
      joinGame = Games.normaliseGameCode(rawJoinGame)
      maybeGame <- appContext.db.lookupGame(joinGame.gameCode)
      rawGameDb <- MonadThrow[F].fromOption(
        maybeGame,
        Failures(
          s"Game not found for code ${joinGame.gameCode}",
          "could not find game to join, is the code correct?",
        )
      )
      playerDbs <- appContext.db.getPlayers(GameId(rawGameDb.gameId))
      // player/spectator IDs aren't persisted in the game's DB record until the game starts
      // so we patch them in here so we can re-use existing functionality
      gameDb = Games.addPlayerIds(rawGameDb, playerDbs)
      game <- Representations.gameFromDb(gameDb, playerDbs)
      _ <- Games.ensureNotStarted(game)
      connections <- appContext.db.getConnections(game.gameId)
      _ <- Games.ensureNotAlreadyPlaying(connections, appContext.playerAddress)
      _ <- Games.ensureNoDuplicateScreenName(game, joinGame.screenName)
      _ <- Games.ensurePlayerCount(game.players.length)
      now <- appContext.time.now
      player = Games.newPlayer(game.gameId, joinGame.screenName, false, now)
      newGame = Games.addPlayer(game, player)
      addresses = Responses.playerAddresses(connections, player.playerId, appContext.playerAddress)
      response = Responses.welcome(newGame, player, appContext.playerAddress, addresses)
      playerDb = Representations.playerToDb(player)
      _ <- appContext.db.writePlayer(playerDb)
      _ <- appContext.db.putConnection(connectionDb(newGame, player.playerId, appContext.playerAddress))
    } yield response
  }

  /**
   * Configures the game (stacks, timer and player order), Only the host is shown the
   * config UI, and only they can start the game.
   *
   * Might be worth considering allowing the host to be a spectator, as "the house" in future?
   *
   * Players can no longer join after this point, but this might want some thought (especially
   * for spectators).
   */
  def startGame[F[_] : MonadThrow](requestJson: Json, appContext: AppContext[F]): F[Response[GameStatus]] = {
    for {
      startGame <- extractStartGame(requestJson)
      maybeGame <- appContext.db.getGame(startGame.gameId)
      rawGameDb <- MonadThrow[F].fromOption(
        maybeGame,
        Failures(
          s"Cannot start game, game ID not found", "couldn't find game to start",
        )
      )
      playerDbs <- appContext.db.getPlayers(GameId(rawGameDb.gameId))
      gameDb = Games.addPlayerIds(rawGameDb, playerDbs)
      rawGame <- Representations.gameFromDb(gameDb, playerDbs)
      _ <- Games.ensureNotStarted(rawGame)
      host <- Games.ensureHost(rawGame.players, startGame.playerKey)
      _ <- Games.ensureStartingPlayerCount(rawGame.players.length)
      now <- appContext.time.now
      startedGame = Games.start(rawGame, now, startGame.initialSmallBlind, startGame.timerConfig, startGame.startingStack, startGame.playerOrder)
      startedGameDb = Representations.gameToDb(startedGame)
      playerDbs = Representations.allPlayerDbs(startedGame.players)
      // update all players with dealt cards, stack size etc
      _ <- playerDbs.traverse(appContext.db.writePlayer)
      // persist started game
      _ <- appContext.db.writeGame(startedGameDb)
      addresses <- requesterConnection(startedGame, host.playerId, appContext)
    } yield Responses.gameStatuses(startedGame, GameStartedSummary(), addresses)
  }

  def bet[F[_] : MonadThrow](requestJson: Json, appContext: AppContext[F]): F[Response[GameStatus]] = {
    for {
      bet <- extractBet(requestJson)
      maybeGame <- appContext.db.getGame(bet.gameId)
      gameDb <- MonadThrow[F].fromOption(
        maybeGame,
        Failures(
          s"Cannot bet, game ID not found", "couldn't find the game",
        )
      )
      playerDbs <- appContext.db.getPlayers(GameId(gameDb.gameId))
      rawGame <- Representations.gameFromDb(gameDb, playerDbs)
      _ <- Games.ensureStarted(rawGame)
      rawPlayer <- Games.ensurePlayerKey(rawGame.players, bet.playerId, bet.playerKey)
      _ <- Games.ensureActive(rawGame.inTurn, bet.playerId)
      betResult <- PlayerActions.bet(rawGame, bet.betAmount, rawPlayer)
      (newGame, updatedPlayerIds, action) = betResult
      // obtain DB representations for persistence
      updatedPlayerDbs <- Representations.filteredPlayerDbs(newGame.players, updatedPlayerIds)
      newGameDb = Representations.gameToDb(newGame)
      // save this player
      _ <- updatedPlayerDbs.traverse(appContext.db.writePlayer)
      // save game
      _ <- appContext.db.writeGame(newGameDb)
      addresses <- requesterConnection(newGame, bet.playerId, appContext)
    } yield Responses.gameStatuses(newGame, action, addresses)
  }

  def check[F[_] : MonadThrow](requestJson: Json, appContext: AppContext[F]): F[Response[GameStatus]] = {
    for {
      check <- extractCheck(requestJson)
      maybeGame <- appContext.db.getGame(check.gameId)
      gameDb <- MonadThrow[F].fromOption(
        maybeGame,
        Failures(
          s"Cannot check, game ID not found", "couldn't find the game",
        )
      )
      playerDbs <- appContext.db.getPlayers(GameId(gameDb.gameId))
      rawGame <- Representations.gameFromDb(gameDb, playerDbs)
      _ <- Games.ensureStarted(rawGame)
      player <- Games.ensurePlayerKey(rawGame.players, check.playerId, check.playerKey)
      _ <- Games.ensureActive(rawGame.inTurn, check.playerId) // TODO: allow off-turn checks?
      newGame <- PlayerActions.check(rawGame, player)
      // obtain DB representations for persistence
      updatedPlayerDbs <- Representations.filteredPlayerDbs(newGame.players, Set(check.playerId))
      newGameDb = Representations.gameToDb(newGame)
      // save this player
      _ <- updatedPlayerDbs.traverse(appContext.db.writePlayer)
      // save game
      _ <- appContext.db.writeGame(newGameDb)
      addresses <- requesterConnection(newGame, check.playerId, appContext)
    } yield Responses.gameStatuses(newGame, CheckSummary(check.playerId), addresses)
  }

  def fold[F[_] : MonadThrow](requestJson: Json, appContext: AppContext[F]): F[Response[GameStatus]] = {
    for {
      fold <- extractFold(requestJson)
      maybeGame <- appContext.db.getGame(fold.gameId)
      gameDb <- MonadThrow[F].fromOption(
        maybeGame,
        Failures(
          s"Cannot fold, game ID not found", "couldn't find the game",
        )
      )
      playerDbs <- appContext.db.getPlayers(GameId(gameDb.gameId))
      rawGame <- Representations.gameFromDb(gameDb, playerDbs)
      _ <- Games.ensureStarted(rawGame)
      player <- Games.ensurePlayerKey(rawGame.players, fold.playerId, fold.playerKey)
      _ <- Games.ensureActive(rawGame.inTurn, fold.playerId) // TODO: allow off-turn folds?
      newGame = PlayerActions.fold(rawGame, player)
      // obtain DB representations for persistence
      updatedPlayerDbs <- Representations.filteredPlayerDbs(newGame.players, Set(fold.playerId))
      newGameDb = Representations.gameToDb(newGame)
      // save this player
      _ <- updatedPlayerDbs.traverse(appContext.db.writePlayer)
      // save game
      _ <- appContext.db.writeGame(newGameDb)
      addresses <- requesterConnection(newGame, fold.playerId, appContext)
    } yield Responses.gameStatuses(newGame, FoldSummary(fold.playerId), addresses)
  }

  /**
   * Signals to the dealer that it is time for the next cards.
   *
   * If stacks are tracked this is only required between rounds, but in card-only games
   * each phase needs to be triggered.
   *
   * TODO: should this be split into separate endpoints with specific response formats?
   *       the showdown endpoint is the tricky one now, but 'advance round' could be separate as well
   *
   * TODO: game setting for auto-advance?
   *       perhaps separate settings for auto-advancing phase / showdown / round
   */
  def advancePhase[F[_] : MonadThrow](requestJson: Json, appContext: AppContext[F]): F[Response[Message]] = {
    for {
      advancePhase <- extractAdvancePhase(requestJson)
      maybeGame <- appContext.db.getGame(advancePhase.gameId)
      rawGameDb <- MonadThrow[F].fromOption(
        maybeGame,
        Failures(
          s"Cannot advance phase, game ID not found", "couldn't find the game",
        )
      )
      playerDbs <- appContext.db.getPlayers(GameId(rawGameDb.gameId))
      game <- Representations.gameFromDb(rawGameDb, playerDbs)
      _ <- Games.ensureStarted(game)
      admin <- Games.ensureAdmin(game.players, advancePhase.playerKey)
      now <- appContext.time.now
      // TODO: recursively call this operation if we are auto-advancing?
      // TODO: move rng's application here - get next state and pass it into pure functions.
      advanceResult <- PlayerActions.advancePhase(game, now, appContext.rng)
      (updatedGame, updatedPlayers, winnings) = advanceResult
      newGameDb = Representations.gameToDb(updatedGame)
      // only do DB updates for players that have changed
      updatedPlayerDbs <- Representations.filteredPlayerDbs(updatedGame.players, updatedPlayers)
      _ <- updatedPlayerDbs.traverse(appContext.db.writePlayer)
      _ <- appContext.db.writeGame(newGameDb)
      addresses <- requesterConnection(updatedGame, admin.playerId, appContext)
    } yield {
      // TODO: this is too much logic for the controller
      winnings match {
        case Some((playerWinnings, potWinnings)) =>
          Responses.roundWinnings(updatedGame, potWinnings, playerWinnings, addresses)
        case None =>
          Responses.gameStatuses(updatedGame, AdvancePhaseSummary(), addresses)
      }
    }
  }

  /**
   * Allows control of the blinds, manually or via the timer.
   *
   * This is commonly a manual update (for manual blind games) or play/pause (for timed games),
   * but may also be editing the phases or timer progress in timed games.
   *
   * Pausing / playing is done by setting the optional pauseTime and by faking the start time, respectively.
   */
  def updateBlind[F[_] : MonadThrow](requestJson: Json, appContext: AppContext[F]): F[Response[GameStatus]] = {
    for {
      updateBlind <- extractUpdateBlind(requestJson)
      maybeGame <- appContext.db.getGame(updateBlind.gameId)
      rawGameDb <- MonadThrow[F].fromOption(
        maybeGame,
        Failures(
          s"Cannot update blind, game ID not found", "couldn't find the game",
        )
      )
      playerDbs <- appContext.db.getPlayers(GameId(rawGameDb.gameId))
      game <- Representations.gameFromDb(rawGameDb, playerDbs)
      _ <- Games.ensureStarted(game)
      admin <- Games.ensureAdmin(game.players, updateBlind.playerKey)
      now <- appContext.time.now
      updatedGame <- PlayerActions.updateBlind(game, updateBlind, now)
      newGameDb = Representations.gameToDb(updatedGame)
      action <- Games.updateBlindAction(updateBlind)
      _ <- appContext.db.writeGame(newGameDb)
      // this endpoint won't update players so there's no need to save them
      addresses <- requesterConnection(updatedGame, admin.playerId, appContext)
    } yield Responses.gameStatuses(updatedGame, action, addresses)
  }

  /**
   * Asks for a game update.
   * Also serves as a reconnect endpoint that updates a player's address.
   *
   * Only available for valid connected players.
   */
  def ping[F[_] : MonadThrow](requestJson: Json, appContext: AppContext[F]): F[Response[GameStatus]] = {
    for {
      pingRequest <- extractPing(requestJson)
      // fetch player / game data
      gameDbOpt <- appContext.db.getGame(pingRequest.gameId)
      rawGameDb <- Games.requireGame(gameDbOpt, pingRequest.gameId.gid)
      playerDbs <- appContext.db.getPlayers(pingRequest.gameId)
      // we remove duplicates, so it is safe to re-add playerDbs here
      // this addresses pings when the game has not yet started (and players have not been added)
      gameDb = Games.addPlayerIds(rawGameDb, playerDbs)
      game <- Representations.gameFromDb(gameDb, playerDbs)
      // TODO: handle players or spectators here
      //       maybe check if requester is a player / spectator and delegate accordingly?
      player <- Games.ensurePlayerKey(game.players, pingRequest.playerId, pingRequest.playerKey)
      _ <- requesterConnection(game, player.playerId, appContext)
      message = Representations.gameStatus(game, player, NoActionSummary())
    } yield Responses.justRespond(message, appContext.playerAddress)
  }

  /**
   * Saves the requester's connection if the game doesn't already have it, and returns every player's addresses.
   */
  private def requesterConnection[F[_] : MonadThrow](game: Game, playerId: PlayerId, appContext: AppContext[F]): F[Map[PlayerId, Set[PlayerAddress]]] = {
    for {
      connections <- appContext.db.getConnections(game.gameId)
      alreadyConnected = connections.exists(c => c.address == appContext.playerAddress.address && c.playerId == playerId.pid)
      _ <-
        if (alreadyConnected) MonadThrow[F].unit
        else appContext.db.putConnection(connectionDb(game, playerId, appContext.playerAddress))
    } yield Responses.playerAddresses(connections, playerId, appContext.playerAddress)
  }

  private def connectionDb(game: Game, playerId: PlayerId, playerAddress: PlayerAddress): ConnectionDb = {
    ConnectionDb(game.gameId.gid, playerAddress.address, playerId.pid, game.expiry)
  }

  // TODO: split logic for players / spectators

  def playerPing[F[_] : MonadThrow](requestJson: Json, appContext: AppContext[F]): F[(Message, PlayerDb)] = {
    ???
  }

  def spectatorPing[F[_] : MonadThrow](requestJson: Json, appContext: AppContext[F]): F[(Message, PlayerDb)] = {
    ???
  }

  /**
   * This endpoint does nothing here, but executing this function
   * wakes the container so that subsequent requests load quickly.
   */
  def wake[F[_] : MonadThrow](appContext: AppContext[F]): F[Response[Status]] = {
    MonadThrow[F].pure {
      Responses.ok(appContext.playerAddress)
    }
  }
}
