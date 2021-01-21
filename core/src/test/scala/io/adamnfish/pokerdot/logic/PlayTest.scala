package io.adamnfish.pokerdot.logic

import io.adamnfish.pokerdot.{TestDates, TestHelpers}
import org.scalatest.freespec.AnyFreeSpec
import io.adamnfish.pokerdot.logic.Play._
import io.adamnfish.pokerdot.logic.Cards.RichRank
import io.adamnfish.pokerdot.logic.Games.newPlayer
import io.adamnfish.pokerdot.models.{Ace, Clubs, Diamonds, Flop, GameId, Hole, PlayerAddress, PreFlop, River, Showdown, Three, Turn, Two}
import org.scalacheck.Gen
import org.scalatest.EitherValues
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks


class PlayTest extends AnyFreeSpec with Matchers with ScalaCheckDrivenPropertyChecks with TestHelpers with EitherValues {
  "generateRound" - {
    "generates different cards for different seeds" in {
      forAll { (seed: Long) =>
        val round1 = generateRound(PreFlop, 0, seed)
        val round2 = generateRound(PreFlop, 0, seed + 1)
        round1 should not equal round2
      }
    }

    "generates the same cards from the same seeds" in {
      forAll { seed: Long =>
        val round1 = generateRound(PreFlop, 0, seed)
        val round2 = generateRound(PreFlop, 0, seed)
        round1 shouldEqual round2
      }
    }

    "there are no duplicate cards in a generated round" in {
      forAll { seed: Long =>
        val round = generateRound(PreFlop, 0, seed)
        val cards = List(round.burn1, round.flop1, round.flop2, round.flop3, round.burn2, round.turn, round.burn3, round.river)
        cards shouldEqual cards.distinct
      }
    }

    "uses the provided small blind amount" in {
      forAll { smallBlind: Int =>
        val round = generateRound(PreFlop, smallBlind, 0L)
        round.smallBlind shouldEqual smallBlind
      }
    }

    "uses the provided phase" in {
      forAll(Gen.oneOf(PreFlop, Flop ,Turn ,River, Showdown)) { phase =>
        val round = generateRound(phase, 0, 0L)
        round.phase shouldEqual phase
      }
    }
  }

  "deckOrder" - {
    "returns the same deck order for the same seed" in {
      forAll { seed: Long =>
        val deck1 = deckOrder(seed)
        val deck2 = deckOrder(seed)
        deck1 shouldEqual deck2
      }
    }

    "returns different decks for different seeds" in {
      // this is very unlikely to fail accidentally,
      // but it is not impossible that three different seeds produce the same deck
      forAll { (seed1: Long, seed2: Long, seed3: Long) =>
        whenever (seed1 != seed2 && seed2 != seed3 && seed3 != seed1) {
          val deck1 = deckOrder(seed1)
          val deck2 = deckOrder(seed2)
          val deck3 = deckOrder(seed3)
          (deck1 == deck2 && deck2 == deck3 && deck3 == deck1) shouldEqual false
        }
      }
    }
  }

  "dealHoles" - {
    val gameId = GameId("game-id")
    val players = List(
      newPlayer(gameId, "player-1", false, PlayerAddress("player-address-1"), TestDates),
      newPlayer(gameId, "player-2", false, PlayerAddress("player-address-2"), TestDates),
      newPlayer(gameId, "player-3", false, PlayerAddress("player-address-3"), TestDates),
      newPlayer(gameId, "player-4", false, PlayerAddress("player-address-4"), TestDates),
      newPlayer(gameId, "player-5", false, PlayerAddress("player-address-5"), TestDates),
      newPlayer(gameId, "player-6", false, PlayerAddress("player-address-6"), TestDates),
    )

    "deals the same cards to each player each time, with the same seed" in {
      forAll { seed: Long =>
        val deck = deckOrder(seed)
        val players1 = dealHoles(players, deck)
        val players2 = dealHoles(players, deck)
        players1.map(_.hole) shouldEqual players2.map(_.hole)
      }
    }

    "the round's cards are not dealt to players" in {
      forAll { seed: Long =>
        val round = generateRound(PreFlop, 0, seed)
        val deck = deckOrder(seed)
        val allPlayerCards = dealHoles(players, deck)
          .flatMap { player =>
            player.hole.toList
              .flatMap(h => List(h.card1, h.card2))
          }
          .toSet
        val roundCards = Set(
          round.burn1, round.flop1, round.flop2, round.flop3, round.burn2, round.turn, round.burn3, round.river
        )
        allPlayerCards.intersect(roundCards) shouldBe empty
      }
    }

    "players are never dealt the same cards as each other" in {
      forAll { seed: Long =>
        val deck = deckOrder(seed)
        val allPlayerCards = dealHoles(players, deck)
          .flatMap { player =>
            player.hole.toList
              .flatMap(h => List(h.card1, h.card2))
          }
        allPlayerCards shouldEqual allPlayerCards.distinct
      }
    }
  }

  "lookupHoles" - {
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
      lookupHoles(players) shouldEqual List(
        player1.playerId -> Hole(Ace of Clubs, Ace of Diamonds),
        player2.playerId -> Hole(Two of Clubs, Two of Diamonds),
        player3.playerId -> Hole(Three of Clubs, Three of Diamonds),
      )
    }

    "excludes busted players" in {
      val players = List(
        player1, player2, player3.copy(busted = true)
      )
      lookupHoles(players) shouldEqual List(
        player1.playerId -> Hole(Ace of Clubs, Ace of Diamonds),
        player2.playerId -> Hole(Two of Clubs, Two of Diamonds),
      )
    }

    "excludes folded players" in {
      val players = List(
        player1, player2, player3.copy(folded = true)
      )
      lookupHoles(players) shouldEqual List(
        player1.playerId -> Hole(Ace of Clubs, Ace of Diamonds),
        player2.playerId -> Hole(Two of Clubs, Two of Diamonds),
      )
    }
  }

  "playerIsYetToAct" - {
    val player =
      Games.newPlayer(GameId("game-id"), "player-name", false, PlayerAddress("player-address"), TestDates)
        .copy(
          hole = Some(Hole(Ace of Clubs, Ace of Diamonds)),
          bet = 100,
          stack = 1000,
        )

    "unmodified test player" - {
      "needs to act when bet amount equals their own input" in {
        playerIsYetToAct(100)(player) shouldEqual true
      }

      "needs to act when bet amount exceeds their own input" in {
        playerIsYetToAct(200)(player) shouldEqual true
      }
    }

    "checked player" - {
      "does not need to act when bet amount equals their own input" in {
        playerIsYetToAct(100)(player.copy(checked = true)) shouldEqual false
      }

      "needs to act when bet amount exceeds their own input" in {
        playerIsYetToAct(200)(player.copy(checked = true)) shouldEqual true
      }
    }

    "a folded player does not need to act for any amount" in {
      forAll { (betAmount: Int) =>
        playerIsYetToAct(betAmount)(player.copy(folded = true)) shouldEqual false
      }
    }

    "a busted player does not need to act for any amount" in {
      forAll { (betAmount: Int) =>
        playerIsYetToAct(betAmount)(player.copy(busted = true)) shouldEqual false
      }
    }
  }

  "currentBetAmount" - {
    "returns the highest bet amount of all players" in {
      forAll { (b1: Int, b2: Int, b3: Int) =>
        val players = List(
          newPlayer(GameId("game-id"), "player-1", false, PlayerAddress("pa-1"), TestDates)
            .copy(bet = b1),
          newPlayer(GameId("game-id"), "player-2", false, PlayerAddress("pa-2"), TestDates)
            .copy(bet = b2),
          newPlayer(GameId("game-id"), "player-3", false, PlayerAddress("pa-3"), TestDates)
            .copy(bet = b3),
        )
        val result = currentBetAmount(players)
        result should (be >= b1 and be >= b2 and be >= b3)
      }
    }
  }

  "nextPlayer" - {
    "when a player is already active" - {
      "returns the next player" in {

      }

      "skips a folded player" in {

      }

      "skips a busted player" in {

      }

      "wraps around the players list to find the next" in {

      }

      "wraps around the players list when skipping a folded player" in {

      }

      "wraps around the players list when skipping a busted player" in {

      }
    }

    "when no player is currently active" - {
      "activates the player to the left of the player that is on the button" in {

      }
    }
  }
}
