package io.adamnfish.pokerdot

import io.adamnfish.pokerdot.logic.Utils.{Attempt, RichList, RichEither}
import io.adamnfish.pokerdot.logic.{Games, Play, Representations, Responses}
import io.adamnfish.pokerdot.models.Serialisation.{parseCreateGameRequest, parseJoinGameRequest}
import io.adamnfish.pokerdot.models._
import io.adamnfish.pokerdot.utils.Rng
import io.adamnfish.pokerdot.validation.Validation.validate
import io.circe.Json
import zio._


object PokerDot {
  def pokerdot(requestBody: String, appContext: AppContext): Attempt[String] = {
    (for {
      requestJson <- Serialisation.parse(requestBody, "could not understand the request", None)
      operationJson <- IO.fromOption(requestJson.hcursor.downField("operation").focus).mapError(_ =>
        Failures("Request did not include operation field", "Could not understand the request")
      )
      operation <- Serialisation.asAttempt[String](operationJson, "Unexpected operation")
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
      // send status messages
      _ <- response.statuses.toList.ioTraverse { case (address, statusMsg) =>
        appContext.messaging.sendMessage(address, statusMsg)
      }
    } yield operation)
      .tapError { failures =>
        appContext.messaging.sendError(appContext.playerAddress, failures)
      }
  }

  // OPERATIONS

  def createGame(requestJson: Json, appContext: AppContext, initialSeed: Long): Attempt[Response[Welcome]] = {
    for {
      createGame <- parseCreateGameRequest(requestJson) >>= validate
      (_, game) = Play.newGame(createGame.gameName, trackStacks = false).run(initialSeed)
      creator = Play.newPlayer(game.gameId, createGame.screenName, isCreator = true, appContext.playerAddress)
      gameDb = Representations.gameToDb(game)
      creatorDb = Representations.playerToDb(creator)
      response = Responses.welcome(game, creator)
      _ <- appContext.db.writeGame(gameDb)
      _ <- appContext.db.writePlayer(creatorDb)
    } yield response
  }

  /**
   * Allows a player to join a pending game.
   *
   * TODO: this or another operation should allow spectators
   */
  def joinGame(requestJson: Json, appContext: AppContext): Attempt[Response[Welcome]] = {
    for {
      rawJoinGame <- parseJoinGameRequest(requestJson) >>= validate
      joinGame = Games.normaliseGameCode(rawJoinGame)
      maybeGame <- appContext.db.lookupGame(joinGame.gameCode)
      // player IDs aren't persisted until the game starts
      rawGameDb <- Attempt.fromOption(maybeGame, Failures(
        s"Game not found for code ${joinGame.gameCode}",
        "Couldn't find game, is the code correct?",
      ))
      playerDbs <- appContext.db.getPlayers(GameId(rawGameDb.gameId))
      gameDb = Games.addPlayerIds(rawGameDb, playerDbs)
      game <- Representations.gameFromDb(gameDb, playerDbs).attempt
      _ <- Games.ensureNotStarted(game).attempt
      _ <- Games.ensureNotAlreadyPlaying(game, appContext.playerAddress).attempt
      _ <- Games.ensureNoDuplicateScreenName(game, joinGame.screenName).attempt
      player = Play.newPlayer(game.gameId, joinGame.screenName, false, appContext.playerAddress)
      newGame = Games.addPlayer(game, player)
      response = Responses.welcome(newGame, player)
      playerDb = Representations.playerToDb(player)
      _ <- appContext.db.writePlayer(playerDb)
    } yield response
  }

  /**
   * Configures the game (stacks, timer and player order), Only the creator is shown the
   * config UI, and only they can start the game.
   *
   * Might be worth considering allowing the creator to be a spectator, as "the house" in future?
   *
   * Players can no longer join after this point, but this might want some thought (especially
   * for spectators).
   */
  def startGame(requestJson: Json, appContext: AppContext): Attempt[Response[GameStatus]] = {
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
    ???
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
