package io.adamnfish.pokerdot.logic

import io.adamnfish.pokerdot.{TestDates, TestHelpers}
import org.scalatest.freespec.AnyFreeSpec
import io.adamnfish.pokerdot.logic.Play._
import io.adamnfish.pokerdot.logic.Cards.RichRank
import io.adamnfish.pokerdot.models.{Ace, Clubs, Diamonds, GameId, Hole, PlayerAddress, PreFlop, Three, Two}
import org.scalatest.EitherValues
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks


class PlayTest extends AnyFreeSpec with Matchers with ScalaCheckDrivenPropertyChecks with TestHelpers with EitherValues {
  "generateRound" - {
    "generates different cards for different seeds" in {
      forAll { (seed: Long) =>
        val round1 = generateRound(PreFlop).value(seed)
        val round2 = generateRound(PreFlop).value(seed + 1)
        round1 should not equal round2
      }
    }

    "generates the same cards from the same seeds" in {
      forAll { seed: Long =>
        val round1 = generateRound(PreFlop).value(seed)
        val round2 = generateRound(PreFlop).value(seed)
        round1 shouldEqual round2
      }
    }

    "there are no duplicate cards in a generated round" in {
      forAll { seed: Long =>
        val round = generateRound(PreFlop).value(seed)
        val cards = List(round.burn1, round.flop1, round.flop2, round.flop3, round.burn2, round.turn, round.burn3, round.river)
        cards shouldEqual cards.distinct
      }
    }
  }

  "holes" - {
    val player1 =
      Games.newPlayer(GameId("game-id"), "player-1", false, PlayerAddress("address-1"), TestDates)
        .copy(hole = Some(Hole(Ace of Clubs, Ace of Diamonds)))
    val player2 =
      Games.newPlayer(GameId("game-id"), "player-2", false, PlayerAddress("address-2"), TestDates)
        .copy(hole = Some(Hole(Two of Clubs, Two of Diamonds)))
    val player3 =
      Games.newPlayer(GameId("game-id"), "player-3", false, PlayerAddress("address-3"), TestDates)
        .copy(hole = Some(Hole(Three of Clubs, Three of Diamonds)))

    "returns player IDs with their cards" in {
      val players = List(
        player1, player2, player3
      )
      holes(players) shouldEqual List(
        player1.playerId -> Hole(Ace of Clubs, Ace of Diamonds),
        player2.playerId -> Hole(Two of Clubs, Two of Diamonds),
        player3.playerId -> Hole(Three of Clubs, Three of Diamonds),
      )
    }

    "excludes busted players" in {
      val players = List(
        player1, player2, player3.copy(busted = true)
      )
      holes(players) shouldEqual List(
        player1.playerId -> Hole(Ace of Clubs, Ace of Diamonds),
        player2.playerId -> Hole(Two of Clubs, Two of Diamonds),
      )
    }

    "excludes folded players" in {
      val players = List(
        player1, player2, player3.copy(folded = true)
      )
      holes(players) shouldEqual List(
        player1.playerId -> Hole(Ace of Clubs, Ace of Diamonds),
        player2.playerId -> Hole(Two of Clubs, Two of Diamonds),
      )
    }
  }
}
