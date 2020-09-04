package io.adamnfish.pokerdot

import io.adamnfish.pokerdot.logic.{Play, Representations, Responses}
import io.adamnfish.pokerdot.models.{AppContext, Attempt, Failure, Failures, GameStatus, Message, Response, ResultSummary, RoundWinnings, Serialisation, Welcome}
import io.adamnfish.pokerdot.logic.Utils.{Attempt, RichList}
import io.adamnfish.pokerdot.models.Serialisation.parseCreateGameRequest
import io.adamnfish.pokerdot.utils.Rng
import io.circe.Json
import zio._


object PokerDot {
  val runtime = Runtime.default

  def pokerdot(requestBody: String, appContext: AppContext): Exit[Failures, Unit] = {
    val program  = (for {
      requestJson <- Serialisation.parse(requestBody, "could not understand the request", None)
      operationJson <- IO.fromOption(requestJson.hcursor.downField("operation").focus).mapError(_ =>
        Failure("Request did not include operation field", "Could not understand the request").asFailures
      )
      operation <- Serialisation.asAttempt[String](operationJson, "Unexpected operation")
      response <- operation match {
        case "create-game" =>
          createGame(requestJson, appContext, initialSeed = Rng.randomSeed())
        case "join-game" =>
          joinGame(requestJson, appContext)
        case "start-game" =>
          startGame(requestJson, appContext)
        case "update-time" =>
          updateTime(requestJson, appContext)
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
        case _ =>
          Attempt.failAs[Response[GameStatus]](
            Failure(
              s"Unexpected operation: $operation",
              "The request wasn't something I understand"
            ).asFailures
          )
      }
      // send messages
      _ <- response.messages.toList.ioTraverse { case (address, msg) =>
        appContext.messaging.sendMessage(address, msg)
      }
      // send status messages
      _ <- response.statuses.toList.ioTraverse { case (address, statusMsg) =>
        appContext.messaging.sendMessage(address, statusMsg)
      }
    } yield ()).tapError { failures =>
      appContext.messaging.sendError(appContext.playerAddress, failures)
    }
    runtime.unsafeRunSync(program)
  }


  def createGame(requestJson: Json, appContext: AppContext, initialSeed: Long): Attempt[Response[Welcome]] = {
    for {
      // TODO: and validate
      createGame <- parseCreateGameRequest(requestJson)
      (_, game) = Play.newGame(createGame.gameName, trackStacks = true).run(initialSeed)
      creator = Play.newPlayer(game.gameId, createGame.screenName, isCreator = true, appContext.playerAddress)
      gameDb = Representations.gameToDb(game)
      creatorDb = Representations.playerToDb(creator)
      response = Responses.welcome(game, creator)
      _ <- appContext.db.writeGame(gameDb)
      _ <- appContext.db.writePlayer(creatorDb)
    } yield response
  }

  def joinGame(requestJson: Json, appContext: AppContext): Attempt[Response[Welcome]] = {
    ???
  }

  def startGame(requestJson: Json, appContext: AppContext): Attempt[Response[GameStatus]] = {
    ???
  }

  def updateTime(requestJson: Json, appContext: AppContext): Attempt[Response[GameStatus]] = {
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

  def advancePhase(requestJson: Json, appContext: AppContext): Attempt[Response[RoundWinnings]] = {
    ???
  }

  def ping(requestJson: Json, appContext: AppContext): Attempt[Response[GameStatus]] = {
    ???
  }
}
