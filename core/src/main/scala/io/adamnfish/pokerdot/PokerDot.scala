package io.adamnfish.pokerdot

import io.adamnfish.pokerdot.logic.Utils.{Attempt, RichEither, RichList}
import io.adamnfish.pokerdot.logic.{Games, Representations, Responses}
import io.adamnfish.pokerdot.models._
import io.adamnfish.pokerdot.utils.Rng
import io.adamnfish.pokerdot.validation.Validation.{extractCreateGame, extractJoinGame, extractPing, extractStartGame}
import io.circe.Json
import zio._

import java.time.ZonedDateTime


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
          createGame(requestJson, appContext, initialSeed = Rng.randomSeed())
        case "join-game" =>
          joinGame(requestJson, appContext)
        case "start-game" =>
          startGame(requestJson, appContext)
        case "update-timer" =>
          updateTimer(requestJson, appContext)
        case "bid" =>
          bid(requestJson, appContext)
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
      (_, game) = Games.newGame(createGame.gameName, trackStacks = false, appContext.dates).run(initialSeed)
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
      // player IDs aren't persisted until the game starts
      gameDb = Games.addPlayerIds(rawGameDb, playerDbs)
      game <- Representations.gameFromDb(gameDb, playerDbs).attempt
      _ <- Games.ensureNotStarted(game).attempt
      _ <- Games.ensureNotAlreadyPlaying(game, appContext.playerAddress).attempt
      _ <- Games.ensureNoDuplicateScreenName(game, joinGame.screenName).attempt
      _ <- Games.ensurePlayerCount(game).attempt
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


    } yield 1
    ???
  }

  /**
   * Allows control of the timer. Typically this is just play/pause, but may also be editing the phases.
   *
   * Pausing / playing is done by setting the optional pauseTime and by faking the start time, respectively.
   */
  def updateTimer(requestJson: Json, appContext: AppContext): Attempt[Response[GameStatus]] = {
    ???
  }

  def bid(requestJson: Json, appContext: AppContext): Attempt[Response[GameStatus]] = {
    ???
  }

  def check(requestJson: Json, appContext: AppContext): Attempt[Response[GameStatus]] = {
    ???
  }

  def fold(requestJson: Json, appContext: AppContext): Attempt[Response[GameStatus]] = {
    ???
  }

  /**
   * Signals to the dealer that it is time for the next cards.
   *
   * If stacks are tracked this is only required between rounds, but in card-only games
   * each phase needs to be triggered.
   */
  def advancePhase(requestJson: Json, appContext: AppContext): Attempt[Response[RoundWinnings]] = {
    ???
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
      // check player
      player <- Games.ensurePlayerKey(game, pingRequest.playerId, pingRequest.playerKey).attempt
      // logic
      updatedPlayer = Games.updatePlayerAddress(player, appContext.playerAddress)
      // create and save updated player for DB
      updatedPlayerDb = Representations.playerToDb(updatedPlayer)
      message = Representations.gameStatus(game, updatedPlayer, NoActionSummary())
      _ <- appContext.db.writePlayer(updatedPlayerDb)
    } yield Responses.justRespond(message, appContext.playerAddress)
  }

  /**
   * This endpoint does "nothing", but wakes the server so subsequent requests load quickly.
   */
  def wake(appContext: AppContext): Attempt[Response[Status]] = {
    IO.succeed {
      Responses.ok(appContext.playerAddress)
    }
  }
}
