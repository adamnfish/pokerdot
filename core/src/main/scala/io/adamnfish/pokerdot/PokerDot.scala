package io.adamnfish.pokerdot

import io.adamnfish.pokerdot.logic.Utils.{Attempt, RichEither, RichList}
import io.adamnfish.pokerdot.logic.{Games, PlayerActions, Representations, Responses}
import io.adamnfish.pokerdot.models._
import io.adamnfish.pokerdot.services.Database
import io.adamnfish.pokerdot.validation.Validation.{extractAbandonRound, extractAdvancePhase, extractBet, extractCheck, extractCreateGame, extractFold, extractJoinGame, extractPing, extractStartGame, extractUpdateBlind}
import io.circe.Json
import zio._


object PokerDot {
  def pokerdot(requestBody: String, appContext: AppContext): Attempt[String] = {
    (for {
      requestJson <- Serialisation.parse(requestBody, "could not understand the request", None).attempt
      operationJson <- IO.fromOption(requestJson.hcursor.downField("operation").focus).mapError(_ =>
        Failures("Request did not include operation field", "could not understand the request")
      )
      operation <- Serialisation.extractJson[String](operationJson, "unexpected operation").attempt
      response <- operation match {
        case "create-game" =>
          createGame(requestJson, appContext, initialSeed = appContext.rng.randomState())
        case "join-game" =>
          joinGame(requestJson, appContext)
        case "start-game" =>
          startGame(requestJson, appContext)
        case "bet" =>
          bet(requestJson, appContext)
        case "check" =>
          check(requestJson, appContext)
        case "fold" =>
          fold(requestJson, appContext)
        case "advance-phase" =>
          advancePhase(requestJson, appContext)
        case "update-blind" =>
          updateBlind(requestJson, appContext)
        case "abandon-round" =>
          abandonRound(requestJson, appContext)
        // TODO: include admin endpoint to allow manual correction of game state
        case "ping" =>
          ping(requestJson, appContext)
        case "wake" =>
          wake(appContext)
        case _ =>
          Attempt.failAs[Response[GameStatus]](
            Failures(
              s"Unexpected operation: $operation",
              "the request wasn't something I understand"
            )
          )
      }
      // send messages
      allMessages = response.messages.toList ++ response.statuses.toList
      _ <- allMessages.ioTraverse { case (address, msg: Message) =>
        appContext.messaging.sendMessage(address, msg)
      }
    } yield operation)
      .tapError { failures =>
        // There are some failure messages that we don't want to send to clients (e.g. failed message delivery).
        // It's not urgent, but prevents cluttering a user's experience with irrelevant failure information.
        failures.externalFailures match {
          case Nil =>
            // if all the messages were 'internal' then there's no need to send a failure message
            IO.unit
          case externalFailures =>
            appContext.messaging.sendError(appContext.playerAddress, failures.copy(failures = externalFailures))
        }
      }
  }

  // OPERATIONS

  def createGame(requestJson: Json, appContext: AppContext, initialSeed: Long): Attempt[Response[Welcome]] = {
    for {
      createGame <- extractCreateGame(requestJson).attempt
      rawGame = Games.newGame(createGame.gameName, trackStacks = false, appContext.clock, initialSeed)
      uniqueGameCode <- Games.makeUniquePrefix(rawGame.gameId, appContext.db, Database.checkUniquePrefix)
      game = rawGame.copy(gameCode = uniqueGameCode)
      host = Games.newPlayer(game.gameId, createGame.screenName, isHost = true, appContext.playerAddress, appContext.clock)
      gameWithHost = Games.addPlayer(game, host)
      gameDb = Representations.gameToDb(gameWithHost)
      hostDb = Representations.playerToDb(host)
      response = Responses.welcome(gameWithHost, host, appContext.playerAddress)
      _ <- appContext.db.writeGame(gameDb)
      _ <- appContext.db.writePlayer(hostDb)
    } yield response
  }

  /**
   * Allows a player to join a pending game.
   *
   * TODO: this or another operation should allow spectators
   */
  def joinGame(requestJson: Json, appContext: AppContext): Attempt[Response[Welcome]] = {
    for {
      rawJoinGame <- extractJoinGame(requestJson).attempt
      joinGame = Games.normaliseGameCode(rawJoinGame)
      maybeGame <- appContext.db.lookupGame(joinGame.gameCode)
      rawGameDb <- Attempt.fromOption(maybeGame, Failures(
        s"Game not found for code ${joinGame.gameCode}",
        "could not find game to join, is the code correct?",
      ))
      playerDbs <- appContext.db.getPlayers(GameId(rawGameDb.gameId))
      // player/spectator IDs aren't persisted in the game's DB record until the game starts
      // so we patch them in here so we can re-use existing functionality
      gameDb = Games.addPlayerIds(rawGameDb, playerDbs)
      game <- Representations.gameFromDb(gameDb, playerDbs).attempt
      _ <- Games.ensureNotStarted(game).attempt
      _ <- Games.ensureNotAlreadyPlaying(game.players, appContext.playerAddress).attempt
      _ <- Games.ensureNoDuplicateScreenName(game, joinGame.screenName).attempt
      _ <- Games.ensurePlayerCount(game.players.length).attempt
      player = Games.newPlayer(game.gameId, joinGame.screenName, false, appContext.playerAddress, appContext.clock)
      newGame = Games.addPlayer(game, player)
      response = Responses.welcome(newGame, player, appContext.playerAddress)
      playerDb = Representations.playerToDb(player)
      _ <- appContext.db.writePlayer(playerDb)
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
  def startGame(requestJson: Json, appContext: AppContext): Attempt[Response[GameStatus]] = {
    for {
      startGame <- extractStartGame(requestJson).attempt
      maybeGame <- appContext.db.getGame(startGame.gameId)
      rawGameDb <- Attempt.fromOption(maybeGame, Failures(
        s"Cannot start game, game ID not found", "couldn't find game to start",
      ))
      playerDbs <- appContext.db.getPlayers(GameId(rawGameDb.gameId))
      gameDb = Games.addPlayerIds(rawGameDb, playerDbs)
      rawGame <- Representations.gameFromDb(gameDb, playerDbs).attempt
      _ <- Games.ensureNotStarted(rawGame).attempt
      _ <- Games.ensureHost(rawGame.players, startGame.playerKey).attempt
      _ <- Games.ensureStartingPlayerCount(rawGame.players.length).attempt
      now = appContext.clock.now()
      startedGame = Games.start(rawGame, now, startGame.initialSmallBlind, startGame.timerConfig, startGame.startingStack, startGame.playerOrder)
      startedGameDb = Representations.gameToDb(startedGame)
      playerDbs = Representations.allPlayerDbs(startedGame.players)
      // update all players with dealt cards, stack size etc
      _ <- playerDbs.ioTraverse(appContext.db.writePlayer)
      // persist started game
      _ <- appContext.db.writeGame(startedGameDb)
    } yield Responses.gameStatuses(startedGame, GameStartedSummary(), startGame.playerId, appContext.playerAddress)
  }

  def bet(requestJson: Json, appContext: AppContext): Attempt[Response[GameStatus]] = {
    for {
      bet <- extractBet(requestJson).attempt
      maybeGame <- appContext.db.getGame(bet.gameId)
      gameDb <- Attempt.fromOption(maybeGame, Failures(
        s"Cannot bet, game ID not found", "couldn't find the game",
      ))
      playerDbs <- appContext.db.getPlayers(GameId(gameDb.gameId))
      rawGame <- Representations.gameFromDb(gameDb, playerDbs).attempt
      _ <- Games.ensureStarted(rawGame).attempt
      rawPlayer <- Games.ensurePlayerKey(rawGame.players, bet.playerId, bet.playerKey).attempt
      _ <- Games.ensureActive(rawGame.inTurn, bet.playerId).attempt
      betResult <- PlayerActions.bet(rawGame, bet.betAmount, rawPlayer).attempt
      (newGame, action) = betResult
      // obtain DB representations for persistence
      updatedPlayerDbs = Representations.activePlayerDbs(newGame.players)
      newGameDb = Representations.gameToDb(newGame)
      // save this player
      _ <- updatedPlayerDbs.ioTraverse(appContext.db.writePlayer)
      // save game
      _ <- appContext.db.writeGame(newGameDb)
    } yield Responses.gameStatuses(newGame, action, bet.playerId, appContext.playerAddress)
  }

  def check(requestJson: Json, appContext: AppContext): Attempt[Response[GameStatus]] = {
    for {
      check <- extractCheck(requestJson).attempt
      maybeGame <- appContext.db.getGame(check.gameId)
      gameDb <- Attempt.fromOption(maybeGame, Failures(
        s"Cannot check, game ID not found", "couldn't find the game",
      ))
      playerDbs <- appContext.db.getPlayers(GameId(gameDb.gameId))
      rawGame <- Representations.gameFromDb(gameDb, playerDbs).attempt
      _ <- Games.ensureStarted(rawGame).attempt
      player <- Games.ensurePlayerKey(rawGame.players, check.playerId, check.playerKey).attempt
      _ <- Games.ensureActive(rawGame.inTurn, check.playerId).attempt // TODO: allow off-turn checks?
      newGame <- PlayerActions.check(rawGame, player).attempt
      // obtain DB representations for persistence
      updatedPlayerDbs <- Representations.filteredPlayerDbs(newGame.players, Set(check.playerId)).attempt
      newGameDb = Representations.gameToDb(newGame)
      // save this player
      _ <- updatedPlayerDbs.ioTraverse(appContext.db.writePlayer)
      // save game
      _ <- appContext.db.writeGame(newGameDb)
    } yield Responses.gameStatuses(newGame, CheckSummary(check.playerId), check.playerId, appContext.playerAddress)
  }

  def fold(requestJson: Json, appContext: AppContext): Attempt[Response[GameStatus]] = {
    for {
      fold <- extractFold(requestJson).attempt
      maybeGame <- appContext.db.getGame(fold.gameId)
      gameDb <- Attempt.fromOption(maybeGame, Failures(
        s"Cannot fold, game ID not found", "couldn't find the game",
      ))
      playerDbs <- appContext.db.getPlayers(GameId(gameDb.gameId))
      rawGame <- Representations.gameFromDb(gameDb, playerDbs).attempt
      _ <- Games.ensureStarted(rawGame).attempt
      player <- Games.ensurePlayerKey(rawGame.players, fold.playerId, fold.playerKey).attempt
      _ <- Games.ensureActive(rawGame.inTurn, fold.playerId).attempt // TODO: allow off-turn folds?
      newGame = PlayerActions.fold(rawGame, player)
      // obtain DB representations for persistence
      updatedPlayerDbs <- Representations.filteredPlayerDbs(newGame.players, Set(fold.playerId)).attempt
      newGameDb = Representations.gameToDb(newGame)
      // save this player
      _ <- updatedPlayerDbs.ioTraverse(appContext.db.writePlayer)
      // save game
      _ <- appContext.db.writeGame(newGameDb)
    } yield Responses.gameStatuses(newGame, FoldSummary(fold.playerId), fold.playerId, appContext.playerAddress)
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
  def advancePhase(requestJson: Json, appContext: AppContext): Attempt[Response[Message]] = {
    for {
      advancePhase <- extractAdvancePhase(requestJson).attempt
      maybeGame <- appContext.db.getGame(advancePhase.gameId)
      rawGameDb <- Attempt.fromOption(maybeGame, Failures(
        s"Cannot advance phase, game ID not found", "couldn't find the game",
      ))
      playerDbs <- appContext.db.getPlayers(GameId(rawGameDb.gameId))
      game <- Representations.gameFromDb(rawGameDb, playerDbs).attempt
      _ <- Games.ensureStarted(game).attempt
      _ <- Games.ensureAdmin(game.players, advancePhase.playerKey).attempt
      // TODO: recursively call this operation if we are auto-advancing?
      advanceResult <- PlayerActions.advancePhase(game, appContext.clock, appContext.rng).attempt
      (updatedGame, updatedPlayers, winnings) = advanceResult
      newGameDb = Representations.gameToDb(updatedGame)
      // only do DB updates for players that have changed
      updatedPlayerDbs <- Representations.filteredPlayerDbs(updatedGame.players, updatedPlayers).attempt
      _ <- updatedPlayerDbs.ioTraverse(appContext.db.writePlayer)
      _ <- appContext.db.writeGame(newGameDb)
    } yield {
      // TODO: this is too much logic for the controller
      winnings match {
        case Some((playerWinnings, potWinnings)) =>
          Responses.roundWinnings(updatedGame, potWinnings, playerWinnings, advancePhase.playerId, appContext.playerAddress)
        case None =>
          Responses.gameStatuses(updatedGame, AdvancePhaseSummary(), advancePhase.playerId, appContext.playerAddress)
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
  def updateBlind(requestJson: Json, appContext: AppContext): Attempt[Response[GameStatus]] = {
    for {
      updateBlind <- extractUpdateBlind(requestJson).attempt
      maybeGame <- appContext.db.getGame(updateBlind.gameId)
      rawGameDb <- Attempt.fromOption(maybeGame, Failures(
        s"Cannot update blind, game ID not found", "couldn't find the game",
      ))
      playerDbs <- appContext.db.getPlayers(GameId(rawGameDb.gameId))
      game <- Representations.gameFromDb(rawGameDb, playerDbs).attempt
      _ <- Games.ensureStarted(game).attempt
      _ <- Games.ensureAdmin(game.players, updateBlind.playerKey).attempt
      now = appContext.clock.now()
      updatedGame <- PlayerActions.updateBlind(game, updateBlind, now).attempt
      newGameDb = Representations.gameToDb(updatedGame)
      action <- Games.updateBlindAction(updateBlind).attempt
      _ <- appContext.db.writeGame(newGameDb)
      // this endpoint won't update players so there's no need to save them
    } yield Responses.gameStatuses(updatedGame, action, updateBlind.playerId, appContext.playerAddress)
  }

  /**
   * Abandon the current round. Returns all pots and bets and ends the round.
   *
   * This is useful if, for example, a mistake has happened with the dealing.
   */
  def abandonRound(requestJson: Json, appContext: AppContext): Attempt[Response[GameStatus]] = {
    for {
      abandonRound <- extractAbandonRound(requestJson).attempt
      maybeGame <- appContext.db.getGame(abandonRound.gameId)
      rawGameDb <- Attempt.fromOption(maybeGame, Failures(
        s"Cannot abandon round, game ID not found", "couldn't find the game",
      ))
      playerDbs <- appContext.db.getPlayers(GameId(rawGameDb.gameId))
      game <- Representations.gameFromDb(rawGameDb, playerDbs).attempt
      _ <- Games.ensureStarted(game).attempt
      _ <- Games.ensureAdmin(game.players, abandonRound.playerKey).attempt
      updatedGame = PlayerActions.abandonRound(game, appContext.rng)
      updatedPlayerDbs = Representations.allPlayerDbs(updatedGame.players)
      updatedGameDb = Representations.gameToDb(updatedGame)
      _ <- updatedPlayerDbs.ioTraverse(appContext.db.writePlayer)
      _ <- appContext.db.writeGame(updatedGameDb)
    } yield Responses.gameStatuses(updatedGame, AbandonRoundSummary(), abandonRound.playerId, appContext.playerAddress)
  }

  /**
   * Asks for a game update.
   * Also serves as a reconnect endpoint that updates a player's address.
   *
   * Only available for valid connected players.
   */
  def ping(requestJson: Json, appContext: AppContext): Attempt[Response[GameStatus]] = {
    for {
      pingRequest <- extractPing(requestJson).attempt
      // fetch player / game data
      gameDbOpt <- appContext.db.getGame(pingRequest.gameId)
      rawGameDb <- Games.requireGame(gameDbOpt, pingRequest.gameId.gid).attempt
      playerDbs <- appContext.db.getPlayers(pingRequest.gameId)
      // we remove duplicates, so it is safe to re-add playerDbs here
      // this addresses pings when the game has not yet started (and players have not been added)
      gameDb = Games.addPlayerIds(rawGameDb, playerDbs)
      game <- Representations.gameFromDb(gameDb, playerDbs).attempt
      // TODO: handle players or spectators here
      //       maybe check if requester is a player / spectator and delegate accordingly?
      player <- Games.ensurePlayerKey(game.players, pingRequest.playerId, pingRequest.playerKey).attempt
      // update the player's address, if it has changed
      updatedPlayerOpt = Games.updatePlayerAddress(player, appContext.playerAddress)
      updatedPlayer <- updatedPlayerOpt.fold[Attempt[Player]](IO.succeed(player)) { updatedPlayer =>
        // if player's address has changed, persist change to DB
        val updatedPlayerDb = Representations.playerToDb(updatedPlayer)
        appContext.db.writePlayer(updatedPlayerDb).map(_ => updatedPlayer)
      }
      message = Representations.gameStatus(game, updatedPlayer, NoActionSummary())
    } yield Responses.justRespond(message, appContext.playerAddress)
  }

  // TODO: split logic for players / spectators

  def playerPing(requestJson: Json, appContext: AppContext): Attempt[(Message, PlayerDb)] = {
    ???
  }

  def spectatorPing(requestJson: Json, appContext: AppContext): Attempt[(Message, PlayerDb)] = {
    ???
  }

  /**
   * This endpoint does nothing here, but executing this function
   * wakes the container so that subsequent requests load quickly.
   */
  def wake(appContext: AppContext): Attempt[Response[Status]] = {
    IO.succeed {
      Responses.ok(appContext.playerAddress)
    }
  }
}
