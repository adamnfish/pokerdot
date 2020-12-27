package io.adamnfish.pokerdot.validation

import io.adamnfish.pokerdot.logic.Utils.RichAttempt
import io.adamnfish.pokerdot.models.Serialisation.{parseCreateGameRequest, parseJoinGameRequest, parsePingRequest, parseStartGameRequest}
import io.adamnfish.pokerdot.models.{Attempt, CreateGame, Failure, Failures, JoinGame, Ping, StartGame, TimerLevel}
import io.adamnfish.pokerdot.validation.Validators._
import io.circe.Json
import zio.IO

object Validation {
  private[validation] def validate[A](a: A, context: String, validator: Validator[A]): List[Failure] = {
    validator(a, context)
  }

  private[validation] def asResult[A](a: A, failures: List[Failure]): Either[Failures, A] = {
    failures match {
      case Nil => Right(a)
      case fs => Left(Failures(fs))
    }
  }

  def validate(createGame: CreateGame): Either[Failures, CreateGame] = {
    asResult(createGame,
      validate(createGame.gameName, "game name", sensibleLength) ++
        validate(createGame.screenName, "screen name", sensibleLength)
    )
  }

  def extractCreateGame(json: Json): Either[Failures, CreateGame] = {
    for {
      raw <- parseCreateGameRequest(json)
      validated <- validate(raw)
    } yield validated
  }

  def validate(joinGame: JoinGame): Either[Failures, JoinGame] = {
    asResult(joinGame,
      validate(joinGame.gameCode, "game code", gameCode) ++
        validate(joinGame.screenName, "screen name", sensibleLength)
    )
  }

  def extractJoinGame(json: Json): Either[Failures, JoinGame] = {
    for {
      raw <- parseJoinGameRequest(json)
      validated <- validate(raw)
    } yield validated
  }

  def validate(startGame: StartGame): Either[Failures, StartGame] = {
    asResult(startGame,
      validate(startGame.gameId.gid, "game ID", isUUID) ++
        validate(startGame.playerId.pid, "player ID", isUUID) ++
        validate(startGame.playerKey.key, "player ID", isUUID) ++
        startGame.playerOrder.flatMap(pid => validate(pid.pid, "playerOrder", isUUID)) ++
        startGame.timerConfig.flatMap(tl => validate(tl, "timerLevel", timerLevel))
    )
  }

  def extractStartGame(json: Json): Either[Failures, StartGame] = {
    for {
      raw <- parseStartGameRequest(json)
      validated <- validate(raw)
    } yield validated
  }

  def validate(ping: Ping): Either[Failures, Ping] = {
    asResult(ping,
      validate(ping.gameId.gid, "game ID", isUUID) ++
        validate(ping.playerId.pid, "player ID", isUUID) ++
        validate(ping.playerKey.key, "player ID", isUUID)
    )
  }

  def extractPing(json: Json): Either[Failures, Ping] = {
    for {
      raw <- parsePingRequest(json)
      validated <- validate(raw)
    } yield validated
  }
}
