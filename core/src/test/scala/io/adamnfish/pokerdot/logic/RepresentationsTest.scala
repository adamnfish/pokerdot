package io.adamnfish.pokerdot.logic

import io.adamnfish.pokerdot.TestHelpers
import io.adamnfish.pokerdot.logic.Games.newGame
import io.adamnfish.pokerdot.logic.Representations.{gameFromDb, gameToDb}
import io.adamnfish.pokerdot.services.Dates
import org.scalatest.EitherValues
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks


class RepresentationsTest extends AnyFreeSpec with Matchers with ScalaCheckDrivenPropertyChecks with TestHelpers with EitherValues {
  "games" - {
    "round trips a round's cards correctly" in {
      val game = newGame("game name", trackStacks = false, Dates, 1)
      val gameDb = gameToDb(game)
      val reconstructedGame = gameFromDb(gameDb, Nil).value
      reconstructedGame.round shouldEqual game.round
    }
  }
}
