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

import scala.util.Random


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
      forAll(Gen.oneOf(PreFlop, Flop, Turn, River, Showdown)) { phase =>
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
        whenever(seed1 != seed2 && seed2 != seed3 && seed3 != seed1) {
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
      newPlayer(GameId("game-id"), "player-name", false, PlayerAddress("player-address"), TestDates)
        .copy(
          hole = Some(Hole(Ace of Clubs, Ace of Diamonds)),
          bet = 100,
          stack = 1000,
        )
    val otherPlayer =
      newPlayer(GameId("game-id"), "other-player-name", false, PlayerAddress("other-player-address"), TestDates)
        .copy(
          hole = Some(Hole(Ace of Clubs, Ace of Diamonds)),
          bet = 100,
          stack = 1000,
        )

    "unchecked player" - {
      "needs to act when bet amount equals their own input" in {
        playerIsYetToAct(100, List(player, otherPlayer))(player) shouldEqual true
      }

      "needs to act when bet amount exceeds their own input" in {
        playerIsYetToAct(200, List(player, otherPlayer))(player) shouldEqual true
      }

      "does not need to act if they are all-in" in {
        playerIsYetToAct(2000, List(player, otherPlayer))(
          player.copy(
            stack = 0,
          )
        ) shouldEqual false
      }

      "does not need to act if all other players have folded (if they have at least matched the current bet)" in {
        playerIsYetToAct(100, List(
          player,
          otherPlayer.copy(folded = true),
        ))(
          player
        ) shouldEqual false
      }

      "if all other players are all-in" - {
        val player2 =
          newPlayer(GameId("game-id"), "player-2", false, PlayerAddress("player-2-address"), TestDates)
            .copy(
              hole = Some(Hole(Ace of Clubs, Ace of Diamonds)),
              bet = 100,
              stack = 0,
            )
        val player3 =
          newPlayer(GameId("game-id"), "player-3", false, PlayerAddress("player-3-address"), TestDates)
            .copy(
              hole = Some(Hole(Ace of Clubs, Ace of Diamonds)),
              bet = 90,
              stack = 0,
            )
        val player4 =
          newPlayer(GameId("game-id"), "player-4", false, PlayerAddress("player-4-address"), TestDates)
            .copy(
              hole = Some(Hole(Ace of Clubs, Ace of Diamonds)),
              bet = 90,
              stack = 0,
            )

        "does not need to act if they have equaled the highest all-in player's stake" in {
          playerIsYetToAct(100, List(player, player2, player3, player4))(
            player.copy(bet = 100)
          ) shouldEqual false
        }

        "does not need to act if they have exceeded the highest all-in player's stake" in {
          playerIsYetToAct(150, List(player, player2, player3, player4))(
            player.copy(bet = 150)
          ) shouldEqual false
        }

        "needs to act if they have not exceeded the highest all-in player's stake" in {
          playerIsYetToAct(100, List(player, player2, player3, player4))(
            player.copy(bet = 20)
          ) shouldEqual true
        }
      }
    }

    "checked player" - {
      "does not need to act when bet amount equals their own input" in {
        playerIsYetToAct(100, List(player, otherPlayer))(player.copy(checked = true)) shouldEqual false
      }

      "needs to act when bet amount exceeds their own input" in {
        playerIsYetToAct(200, List(player, otherPlayer))(player.copy(checked = true)) shouldEqual true
      }
    }

    "a folded player does not need to act for any amount" in {
      forAll { (betAmount: Int) =>
        playerIsYetToAct(betAmount, List(player, otherPlayer))(player.copy(folded = true)) shouldEqual false
      }
    }

    "a busted player does not need to act for any amount" in {
      forAll { (betAmount: Int) =>
        playerIsYetToAct(betAmount, List(player, otherPlayer))(player.copy(busted = true)) shouldEqual false
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
    val p1 = newPlayer(GameId("game-id"), "p1", false, PlayerAddress("p1-address"), TestDates)
      .copy(stack = 1000)
    val p2 = newPlayer(GameId("game-id"), "p2", false, PlayerAddress("p2-address"), TestDates)
      .copy(stack = 1000)
    val p3 = newPlayer(GameId("game-id"), "p3", false, PlayerAddress("p3-address"), TestDates)
      .copy(stack = 1000)
    val p4 = newPlayer(GameId("game-id"), "p4", false, PlayerAddress("p4-address"), TestDates)
      .copy(stack = 1000)

    "when a player is already active" - {
      "returns the next player" in {
        nextPlayer(List(p1, p2, p3, p4), Some(p1.playerId), 0) shouldEqual Some(p2.playerId)
      }

      "skips a folded player" in {
        nextPlayer(List(
          p1,
          p2.copy(folded = true),
          p3,
          p4,
        ), Some(p1.playerId), 0) shouldEqual Some(p3.playerId)
      }

      "skips a busted player" in {
        nextPlayer(List(
          p1,
          p2.copy(busted = true),
          p3,
          p4,
        ), Some(p1.playerId), 0) shouldEqual Some(p3.playerId)
      }

      "wraps around the players list to find the next" in {
        nextPlayer(List(p1, p2, p3, p4), Some(p4.playerId), 0) shouldEqual Some(p1.playerId)
      }

      "wraps around the players list when skipping a folded player" in {
        nextPlayer(List(
          p1,
          p2,
          p3,
          p4.copy(folded = true),
        ), Some(p3.playerId), 0) shouldEqual Some(p1.playerId)
      }

      "wraps around the players list when skipping a busted player" in {
        nextPlayer(List(
          p1,
          p2,
          p3,
          p4.copy(busted = true),
        ), Some(p3.playerId), 0) shouldEqual Some(p1.playerId)
      }

      "returns None if no players are eligible to become active" in {
        val ineligiblePlayers = List(p1, p2, p3, p4).map(_.copy(folded = true))
        nextPlayer(ineligiblePlayers, Some(p3.playerId), 0) shouldEqual None
      }
    }

    "when no player is currently active" - {
      "activates the player to the left of the button" - {
        "for button index 0" in {
          nextPlayer(List(p1, p2, p3, p4), None, 0) shouldEqual Some(p2.playerId)
        }

        "for button index 1" in {
          nextPlayer(List(p1, p2, p3, p4), None, 1) shouldEqual Some(p3.playerId)
        }

        "for button index 2" in {
          nextPlayer(List(p1, p2, p3, p4), None, 2) shouldEqual Some(p4.playerId)
        }

        "wraps round to the first player for button index 3" in {
          nextPlayer(List(p1, p2, p3, p4), None, 3) shouldEqual Some(p1.playerId)
        }
      }

      "returns None if no players are eligible to become active" in {
        val ineligiblePlayers = List(p1, p2, p3, p4).map(_.copy(folded = true))
        nextPlayer(ineligiblePlayers, None, 0) shouldEqual None
      }
    }
  }

  "indexWhere" - {
    "returned index is equal to the stdlib's index when present" in {
      forAll { seed: Long =>
        val rng = new Random(seed)
        val shuffled = rng.shuffle(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
        indexWhere(shuffled)(_ == 1) shouldEqual Some(shuffled.indexWhere(_ == 1))
      }
    }

    "returns None if the predicate is not satisfied" in {
      indexWhere(List(1, 2, 3))(_ == 4) shouldEqual None
    }
  }
}
