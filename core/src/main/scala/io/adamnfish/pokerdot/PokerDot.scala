package io.adamnfish.pokerdot

import io.adamnfish.pokerdot.logic.Utils.{Attempt, RichEither, RichList}
import io.adamnfish.pokerdot.logic.{PlayerActions, Games, Representations, Responses}
import io.adamnfish.pokerdot.models._
import io.adamnfish.pokerdot.validation.Validation.{extractAdvancePhase, extractBet, extractCheck, extractCreateGame, extractFold, extractJoinGame, extractPing, extractStartGame, extractUpdateTimer}
import io.circe.Json
import zio._


object PokerDot {
  def pokerdot(requestBody: String, appContext: AppContext): Attempt[String] = {
    (for {
      requestJson <- Serialisation.parse(requestBody, "could not understand the request", None).attempt
      operationJson <- IO.fromOption(requestJson.hcursor.downField("operation").focus).mapError(_ =>
        Failures("Request did not include operation field", "Could not understand the request")
      )
      operation <- Serialisation.extractJson[String](operationJson, "Unexpected operation").attempt
      response <- operation match {
        case "create-game" =>
          createGame(requestJson, appContext, initialSeed = appContext.rng.randomState())
        case "join-game" =>
          joinGame(requestJson, appContext)
        case "start-game" =>
          startGame(requestJson, appContext)
        case "update-timer" =>
          updateTimer(requestJson, appContext)
        case "bet" =>
          bet(requestJson, appContext)
        case "check" =>
          check(requestJson, appContext)
        case "fold" =>
          fold(requestJson, appContext)
        case "advance-phase" =>
          advancePhase(requestJson, appContext)
        case "ping" =>
          ping(requestJson, appContext)
        case "wake" =>
          wake(appContext)
        case _ =>
          Attempt.failAs[Response[GameStatus]](
            Failures(
              s"Unexpected operation: $operation",
              "The request wasn't something I understand"
            )
          )
      }
      // send messages
      _ <- response.messages.toList.ioTraverse { case (address, msg: Message) =>
        appContext.messaging.sendMessage(address, msg)
      }
    } yield operation)
      .tapError { failures =>
        appContext.messaging.sendError(appContext.playerAddress, failures)
      }
  }

  // OPERATIONS

  def createGame(requestJson: Json, appContext: AppContext, initialSeed: Long): Attempt[Response[Welcome]] = {
    for {
      createGame <- extractCreateGame(requestJson).attempt
      game = Games.newGame(createGame.gameName, trackStacks = false, appContext.dates, initialSeed)
      host = Games.newPlayer(game.gameId, createGame.screenName, isHost = true, appContext.playerAddress, appContext.dates)
      gameWithHost = Games.addPlayer(game, host)
      gameDb = Representations.gameToDb(gameWithHost)
      hostDb = Representations.playerToDb(host)
      response = Responses.welcome(gameWithHost, host)
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
        "Couldn't find game, is the code correct?",
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
      player = Games.newPlayer(game.gameId, joinGame.screenName, false, appContext.playerAddress, appContext.dates)
      newGame = Games.addPlayer(game, player)
      response = Responses.welcome(newGame, player)
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
        s"Cannot start game, game ID not found", "Couldn't find game to start",
      ))
      playerDbs <- appContext.db.getPlayers(GameId(rawGameDb.gameId))
      gameDb = Games.addPlayerIds(rawGameDb, playerDbs)
      rawGame <- Representations.gameFromDb(gameDb, playerDbs).attempt
      _ <- Games.ensureNotStarted(rawGame).attempt
      _ <- Games.ensureHost(rawGame.players, startGame.playerKey).attempt
      _ <- Games.ensureStartingPlayerCount(rawGame.players.length).attempt
      now = appContext.dates.now()
      startedGame = Games.start(rawGame, now, startGame.timerConfig, startGame.startingStack)
      startedGameDb = Representations.gameToDb(startedGame)
      playerDbs = Representations.allPlayerDbs(startedGame.players)
      // update all players with dealt cards, stack size etc
      _ <- playerDbs.ioTraverse(appContext.db.writePlayer)
      // persist started game
      _ <- appContext.db.writeGame(startedGameDb)
    } yield Responses.gameStatuses(startedGame, GameStartedSummary())
  }

  def bet(requestJson: Json, appContext: AppContext): Attempt[Response[GameStatus]] = {
    for {
      bet <- extractBet(requestJson).attempt
      maybeGame <- appContext.db.getGame(bet.gameId)
      gameDb <- Attempt.fromOption(maybeGame, Failures(
        s"Cannot bet, game ID not found", "Couldn't find game to start",
      ))
      playerDbs <- appContext.db.getPlayers(GameId(gameDb.gameId))
      rawGame <- Representations.gameFromDb(gameDb, playerDbs).attempt
      _ <- Games.ensureStarted(rawGame).attempt
      rawPlayer <- Games.ensurePlayerKey(rawGame.players, bet.playerId, bet.playerKey).attempt
      _ <- Games.ensureActive(rawGame.inTurn, bet.playerId).attempt
      gameAndPlayer <- PlayerActions.bet(rawGame, bet.betAmount, rawPlayer).attempt
      (newGame, updatedPlayer) = gameAndPlayer
      updatedPlayerSummary = Representations.summarisePlayer(updatedPlayer)
      // obtain DB representations for persistence
      updatedPlayerDbs = Representations.activePlayerDbs(newGame.players)
      newGameDb = Representations.gameToDb(newGame)
      // save this player
      _ <- updatedPlayerDbs.ioTraverse(appContext.db.writePlayer)
      // save game
      _ <- appContext.db.writeGame(newGameDb)
    } yield Responses.gameStatuses(newGame, BetSummary(updatedPlayerSummary))
  }

  def check(requestJson: Json, appContext: AppContext): Attempt[Response[GameStatus]] = {
    for {
      check <- extractCheck(requestJson).attempt
      maybeGame <- appContext.db.getGame(check.gameId)
      gameDb <- Attempt.fromOption(maybeGame, Failures(
        s"Cannot bet, game ID not found", "Couldn't find game to start",
      ))
      playerDbs <- appContext.db.getPlayers(GameId(gameDb.gameId))
      rawGame <- Representations.gameFromDb(gameDb, playerDbs).attempt
      _ <- Games.ensureStarted(rawGame).attempt
      player <- Games.ensurePlayerKey(rawGame.players, check.playerId, check.playerKey).attempt
      _ <- Games.ensureActive(rawGame.inTurn, check.playerId).attempt // TODO: allow off-turn checks?
      gameAndPlayer <- PlayerActions.check(rawGame, player).attempt
      (newGame, updatedPlayer) = gameAndPlayer
      updatedPlayerSummary = Representations.summarisePlayer(updatedPlayer)
      // obtain DB representations for persistence
      updatedPlayerDbs <- Representations.filteredPlayerDbs(newGame.players, Set(check.playerId)).attempt
      newGameDb = Representations.gameToDb(newGame)
      // save this player
      _ <- updatedPlayerDbs.ioTraverse(appContext.db.writePlayer)
      // save game
      _ <- appContext.db.writeGame(newGameDb)
    } yield Responses.gameStatuses(newGame, CheckSummary(updatedPlayerSummary))
  }

  def fold(requestJson: Json, appContext: AppContext): Attempt[Response[GameStatus]] = {
    for {
      fold <- extractFold(requestJson).attempt
      maybeGame <- appContext.db.getGame(fold.gameId)
      gameDb <- Attempt.fromOption(maybeGame, Failures(
        s"Cannot bet, game ID not found", "Couldn't find game to start",
      ))
      playerDbs <- appContext.db.getPlayers(GameId(gameDb.gameId))
      rawGame <- Representations.gameFromDb(gameDb, playerDbs).attempt
      _ <- Games.ensureStarted(rawGame).attempt
      player <- Games.ensurePlayerKey(rawGame.players, fold.playerId, fold.playerKey).attempt
      _ <- Games.ensureActive(rawGame.inTurn, fold.playerId).attempt // TODO: allow off-turn folds?
      gameAndPlayer = PlayerActions.fold(rawGame, player)
      (newGame, updatedPlayer) = gameAndPlayer
      updatedPlayerSummary = Representations.summarisePlayer(updatedPlayer)
      // obtain DB representations for persistence
      updatedPlayerDbs <- Representations.filteredPlayerDbs(newGame.players, Set(fold.playerId)).attempt
      newGameDb = Representations.gameToDb(newGame)
      // save this player
      _ <- updatedPlayerDbs.ioTraverse(appContext.db.writePlayer)
      // save game
      _ <- appContext.db.writeGame(newGameDb)
    } yield Responses.gameStatuses(newGame, FoldSummary(updatedPlayerSummary))
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
        s"Cannot start game, game ID not found", "Couldn't find game to start",
      ))
      playerDbs <- appContext.db.getPlayers(GameId(rawGameDb.gameId))
      game <- Representations.gameFromDb(rawGameDb, playerDbs).attempt
      // fetch game
      _ <- Games.ensureStarted(game).attempt
      _ <- Games.ensureAdmin(game.players, advancePhase.playerKey).attempt
      // TODO: recursively call this operation if we are auto-advancing
      advanceResult <- PlayerActions.advancePhase(game, appContext.rng).attempt
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
          Responses.roundWinnings(updatedGame, potWinnings, playerWinnings)
        case None =>
          Responses.gameStatuses(updatedGame, AdvancePhaseSummary())
      }
    }
  }

  /**
   * Allows control of the timer. Typically this is just play/pause, but may also be editing the phases.
   *
   * Pausing / playing is done by setting the optional pauseTime and by faking the start time, respectively.
   */
  def updateTimer(requestJson: Json, appContext: AppContext): Attempt[Response[GameStatus]] = {
    for {
      updateTimer <- extractUpdateTimer(requestJson).attempt
      // ensure host / admin
      // ensure started
    } yield Responses.tbd() // Responses.gameStatuses(???, ???)
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
      gameDb <- Games.requireGame(gameDbOpt, pingRequest.gameId.gid).attempt
      playerDbs <- appContext.db.getPlayers(pingRequest.gameId)
      game <- Representations.gameFromDb(gameDb, playerDbs).attempt
      // TODO: handle players or spectators here
      // maybe check if requester is a player / spectator and delegate accordingly?
      // check player
      player <- Games.ensurePlayerKey(game.players, pingRequest.playerId, pingRequest.playerKey).attempt
      // logic
      updatedPlayer = Games.updatePlayerAddress(player, appContext.playerAddress)
      // create and save updated player for DB
      updatedPlayerDb = Representations.playerToDb(updatedPlayer)
      message = Representations.gameStatus(game, updatedPlayer, NoActionSummary())
      _ <- appContext.db.writePlayer(updatedPlayerDb)
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
