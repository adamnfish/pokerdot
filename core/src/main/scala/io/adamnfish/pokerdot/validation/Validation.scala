package io.adamnfish.pokerdot.validation

import io.adamnfish.pokerdot.logic.Utils.RichAttempt
import io.adamnfish.pokerdot.models.{Attempt, CreateGame, Failures, JoinGame}
import io.adamnfish.pokerdot.validation.Validators._
import zio.IO

object Validation {
  private[validation] def validate[A](a: A, context: String, validator: Validator[A]): Attempt[Unit] = {
    val failures = validator(a, context)
    if (failures.isEmpty) IO.unit
    else IO.fail(Failures(failures))
  }

  def validate(createGame: CreateGame): Attempt[CreateGame] = {
    (validate(createGame.gameName, "game name", sensibleLength) |!|
      validate(createGame.screenName, "screen name", sensibleLength)
      ).map(Function.const(createGame))
  }

  def validate(joinGame: JoinGame): Attempt[JoinGame] = {
    (validate(joinGame.gameCode, "game code", gameCode) |!|
      validate(joinGame.screenName, "screen name", sensibleLength)
      ).map(Function.const(joinGame))
  }
}
