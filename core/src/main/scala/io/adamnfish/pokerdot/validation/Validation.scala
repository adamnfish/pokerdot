package io.adamnfish.pokerdot.validation

import io.adamnfish.pokerdot.models.Serialisation.{parseAdvancePhaseRequest, parseBetRequest, parseCheckRequest, parseCreateGameRequest, parseFoldRequest, parseJoinGameRequest, parsePingRequest, parseStartGameRequest, parseUpdateBlindRequest}
import io.adamnfish.pokerdot.models._
import io.adamnfish.pokerdot.validation.Validators._
import io.circe.Json


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
      validate(createGame.gameName, "gameName", sensibleLength) ++
        validate(createGame.screenName, "screenName", sensibleLength)
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
      validate(joinGame.gameCode, "gameCode", gameCode) ++
        validate(joinGame.screenName, "screenName", sensibleLength)
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
      validate(startGame.gameId.gid, "gameId", isUUID) ++
        validate(startGame.playerId.pid, "playerId", isUUID) ++
        validate(startGame.playerKey.key, "playerId", isUUID) ++
        validate(startGame.playerOrder, "playerOrder", nonEmptyList[PlayerId]) ++
        startGame.playerOrder.flatMap(pid => validate(pid.pid, "playerOrder", isUUID)) ++
        startGame.startingStack.toList.flatMap { s =>
          if (s <= 0) List(Failure(
            "Empty starting stack provided for game start",
            "Players have to start with something to spend, the initial stack must be more than 0"
          )) else Nil
        } ++
        startGame.initialSmallBlind.toList.flatMap { s =>
          if (s <= 0) List(Failure(
            "Empty initial small blind provided for game start",
            "The blinds have to start at a number larger than 0"
          )) else Nil
        } ++
        startGame.timerConfig
          .map(tls => validate(tls, "timerConfig", nonEmptyList[TimerLevel]))
          .getOrElse(Nil) ++ {
        startGame.startingStack.fold[List[Failure]](Nil) { _ =>
          // if we're tracking stacks then we need to know the blinds
          // this can come from the timer config or explicitly (but not both)
          (startGame.initialSmallBlind, startGame.timerConfig) match {
            case (Some(_), Some(_)) => List(Failure(
              "Small blind and timer config provided for game start",
              "If you have set up a timer then you can't also set the initial small blind.",
            ))
            case (None, None) => List(Failure(
              "Neither small blind nor timer config provided for start game with stacks",
              "For the game to track player money it needs to know the blind amounts. You can specify this directly, or set up a timer that keeps track of blinds throughout the game.",
            ))
            case _ => Nil
          }
        }
      }
    )
  }

  def extractStartGame(json: Json): Either[Failures, StartGame] = {
    for {
      raw <- parseStartGameRequest(json)
      validated <- validate(raw)
    } yield validated
  }

  def validate(bet: Bet): Either[Failures, Bet] = {
    asResult(bet,
      validate(bet.gameId.gid, "gameId", isUUID) ++
        validate(bet.playerId.pid, "playerId", isUUID) ++
        validate(bet.playerKey.key, "playerId", isUUID) ++
        validate(bet.betAmount, "betAmount", greaterThanZero)
    )
  }

  def extractBet(json: Json): Either[Failures, Bet] = {
    for {
      raw <- parseBetRequest(json)
      validated <- validate(raw)
    } yield validated
  }

  def validate(check: Check): Either[Failures, Check] = {
    asResult(check,
      validate(check.gameId.gid, "gameId", isUUID) ++
        validate(check.playerId.pid, "playerId", isUUID) ++
        validate(check.playerKey.key, "playerId", isUUID)
    )
  }

  def extractCheck(json: Json): Either[Failures, Check] = {
    for {
      raw <- parseCheckRequest(json)
      validated <- validate(raw)
    } yield validated
  }

  def validate(fold: Fold): Either[Failures, Fold] = {
    asResult(fold,
      validate(fold.gameId.gid, "gameId", isUUID) ++
        validate(fold.playerId.pid, "playerId", isUUID) ++
        validate(fold.playerKey.key, "playerId", isUUID)
    )
  }

  def extractFold(json: Json): Either[Failures, Fold] = {
    for {
      raw <- parseFoldRequest(json)
      validated <- validate(raw)
    } yield validated
  }

  def validate(advancePhase: AdvancePhase): Either[Failures, AdvancePhase] = {
    asResult(advancePhase,
      validate(advancePhase.gameId.gid, "gameId", isUUID) ++
        validate(advancePhase.playerId.pid, "playerId", isUUID) ++
        validate(advancePhase.playerKey.key, "playerId", isUUID)
    )
  }

  def extractAdvancePhase(json: Json): Either[Failures, AdvancePhase] = {
    for {
      raw <- parseAdvancePhaseRequest(json)
      validated <- validate(raw)
    } yield validated
  }

  def validate(updateBlind: UpdateBlind): Either[Failures, UpdateBlind] = {
    asResult(updateBlind,
      validate(updateBlind.gameId.gid, "gameId", isUUID) ++
        validate(updateBlind.playerId.pid, "playerId", isUUID) ++
        validate(updateBlind.playerKey.key, "playerId", isUUID) ++
        updateBlind.timerLevels
          .map(tls => validate(tls, "timerLevels", nonEmptyList[TimerLevel]))
          .getOrElse(Nil) ++
        updateBlind.smallBlind.toList.flatMap(sb => validate(sb, "smallBlind", positiveInteger))
    )
  }

  def extractUpdateBlind(json: Json): Either[Failures, UpdateBlind] = {
    for {
      raw <- parseUpdateBlindRequest(json)
      validated <- validate(raw)
    } yield validated
  }

  def validate(ping: Ping): Either[Failures, Ping] = {
    asResult(ping,
      validate(ping.gameId.gid, "gameId", isUUID) ++
        validate(ping.playerId.pid, "playerId", isUUID) ++
        validate(ping.playerKey.key, "playerId", isUUID)
    )
  }

  def extractPing(json: Json): Either[Failures, Ping] = {
    for {
      raw <- parsePingRequest(json)
      validated <- validate(raw)
    } yield validated
  }
}
