package io.adamnfish.pokerdot.logic

import io.adamnfish.pokerdot.TestHelpers
import org.scalatest.freespec.AnyFreeSpec
import io.adamnfish.pokerdot.logic.Play._
import io.adamnfish.pokerdot.models.PreFlop
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks


class PlayTest extends AnyFreeSpec with Matchers with ScalaCheckDrivenPropertyChecks with TestHelpers {
  "newGame" - {
    "initialises the basic fields correctly" in {
      forAll { (gameName: String, trackStacks: Boolean, seed: Long) =>
        newGame(gameName, trackStacks).value(seed) should have(
          "gameName" as gameName,
          "players" as Nil,
          "spectators" as Nil,
          "inTurn" as None,
          "button" as 0,
          "started" as false,
          "trackStacks" as trackStacks,
          "timer" as None,
        )
      }
    }

    "sets roundType to pre-flop" in {
      val game = newGame("gameName", true).value(12345L)
      game.round.phase shouldEqual PreFlop
    }

    "cards used for this round are equal if the seed is equal" in {
      forAll { (seed: Long) =>
        val game1 = newGame("gameName", false).value(seed)
        val game2 = newGame("gameName", false).value(seed)
        game1.round should have(
          "burn1" as game2.round.burn1,
          "flop1" as game2.round.flop1,
          "flop2" as game2.round.flop2,
          "flop3" as game2.round.flop3,
          "burn2" as game2.round.burn2,
          "turn" as game2.round.turn,
          "burn3" as game2.round.burn3,
          "river" as game2.round.river,
        )
      }
    }

    "cards used for this round are (probably) different if the seed is different" in {
      // less likely to have clashing cards from multiple distinct seeds
      // this helps prevent intermittent failures when the generator happens to stumble across a clash
      forAll { (seed1: Long, seed2: Long, seed3: Long) =>
        whenever(Set(seed1, seed2, seed3).size == 3) {
          val game1 = newGame("gameName", false).value(seed1)
          val game2 = newGame("gameName", false).value(seed2)
          val game3 = newGame("gameName", false).value(seed3)
          val game4 = newGame("gameName", false).value(seed1 + 1)
          val distinctRounds = Set(game1.round, game2.round, game3.round, game4.round)
          distinctRounds.size > 1
        }
      }
    }

    "cards can be regenerated from the current game seed" in {
      forAll { (seed: Long) =>
        val game = newGame("gameName", false).value(seed)
        val regeneratedRound = generateRound(PreFlop).value(game.seed)
        game.round shouldEqual regeneratedRound
      }
    }
  }
}
