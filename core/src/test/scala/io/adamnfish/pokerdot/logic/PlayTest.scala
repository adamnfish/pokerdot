package io.adamnfish.pokerdot.logic

import io.adamnfish.pokerdot.{TestDates, TestHelpers}
import org.scalatest.freespec.AnyFreeSpec
import io.adamnfish.pokerdot.logic.Play._
import io.adamnfish.pokerdot.logic.Cards.RichRank
import io.adamnfish.pokerdot.logic.Games.newPlayer
import io.adamnfish.pokerdot.models.{Ace, BigBlind, Clubs, Diamonds, Flop, GameId, Hole, NoBlind, Player, PlayerAddress, PreFlop, River, Showdown, SmallBlind, Three, Turn, Two}
import org.scalacheck.Gen
import org.scalatest.{EitherValues, OptionValues}
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.util.Random


class PlayTest extends AnyFreeSpec with Matchers with ScalaCheckDrivenPropertyChecks with TestHelpers with EitherValues with OptionValues {
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

  "currentRaiseAmount" - {
    val player1 = newPlayer(GameId("game-id"), "player-1", false, PlayerAddress("pa-1"), TestDates)
    val player2 = newPlayer(GameId("game-id"), "player-2", false, PlayerAddress("pa-2"), TestDates)
    val player3 = newPlayer(GameId("game-id"), "player-3", false, PlayerAddress("pa-3"), TestDates)

    "returns 0 if there are no bets" in {
      currentRaiseAmount(Nil) shouldEqual 0
    }

    "returns the only bet if there is only one bet" in {
      forAll(Gen.choose(0, 1000)) { n =>
        whenever(n > 0) {
          currentRaiseAmount(List(player1.copy(bet = n))) shouldEqual 0
        }
      }
    }

    "returns 0 if there only bet if there is only one (distinct) bet" in {
      forAll(Gen.choose(0, 1000)) { n =>
        whenever(n > 0) {
          currentRaiseAmount(List(
            player1.copy(bet = n),
            player2.copy(bet = n),
            player3.copy(bet = n),
          )) shouldEqual 0
        }
      }
    }

    "returns the difference between the two largest bets" in {
      forAll(Gen.choose(0, 1000), Gen.choose(1, 100), Gen.choose(1, 100)) { (n, d1, d2) =>
        whenever(n > 0 && d1 > 0 && d2 > 0) {
          currentRaiseAmount(List(
            player1.copy(bet = n),
            player2.copy(bet = n + d1),
            player3.copy(bet = n + d1 + d2),
          )) shouldEqual d2
        }
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

  "nextDealerAndBlinds" - {
    val gameId = GameId("game-id")
    val player1 = newPlayer(gameId, "player-1", false, PlayerAddress("player-address-1"), TestDates)
    val player2 = newPlayer(gameId, "player-2", false, PlayerAddress("player-address-2"), TestDates)
    val player3 = newPlayer(gameId, "player-3", false, PlayerAddress("player-address-3"), TestDates)
    val player4 = newPlayer(gameId, "player-4", false, PlayerAddress("player-address-4"), TestDates)
    val player5 = newPlayer(gameId, "player-5", false, PlayerAddress("player-address-5"), TestDates)
    val player6 = newPlayer(gameId, "player-6", false, PlayerAddress("player-address-6"), TestDates)
    val smallBlind = 5

    "for a typical case" - {
      "the button moves ahead one" in {
        val (newButtonIndex, _) = nextDealerAndBlinds(
          List(
            player1,
            player2.copy(blind = SmallBlind),
            player3.copy(blind = BigBlind),
            player4,
          ), 0, smallBlind
        )
        newButtonIndex shouldEqual 1
      }

      "moves blinds ahead one" in {
        val (_, players) = nextDealerAndBlinds(
          List(
            player1,
            player2.copy(blind = SmallBlind),
            player3.copy(blind = BigBlind),
            player4,
          ), 0, smallBlind
        )
        players.map(_.blind) shouldEqual List(NoBlind, NoBlind, SmallBlind, BigBlind)
      }
    }

    "heads-up" - {
      "dealer is always small blind" in {
        forAll { b: Boolean =>
          val (newButtonIndex, players) =
            if (b) nextDealerAndBlinds(List(
              player1.copy(blind = BigBlind),
              player2.copy(blind = SmallBlind),
            ), 1, smallBlind)
            else nextDealerAndBlinds(List(
              player1.copy(blind = SmallBlind),
              player2.copy(blind = BigBlind),
            ), 0, smallBlind)
          players(newButtonIndex).blind shouldEqual SmallBlind
        }
      }

      "non-dealer is always big blind" in {
        forAll { b: Boolean =>
          val (newButtonIndex, players) =
            if (b) nextDealerAndBlinds(List(
              player1.copy(blind = BigBlind),
              player2.copy(blind = SmallBlind),
            ), 1, smallBlind)
            else nextDealerAndBlinds(List(
              player1.copy(blind = SmallBlind),
              player2.copy(blind = BigBlind),
            ), 0, smallBlind)
          players(1 - newButtonIndex).blind shouldEqual BigBlind
        }
      }

      "the big blind moves, and dealer is always small blind in these examples" - {
        "normal for heads-up" in {
          val (_, players) = nextDealerAndBlinds(List(
            player1.copy(blind = BigBlind),
            player2.copy(blind = SmallBlind),
            player3.copy(busted = true),
            player4.copy(busted = true),
          ), 1, smallBlind)
          players.map(_.blind) shouldEqual List(SmallBlind, BigBlind, NoBlind, NoBlind)
        }

        "BB jumping busted players (dealer went bust)" in {
          val (newButtonPosition, players) = nextDealerAndBlinds(List(
            player1.copy(blind = BigBlind),
            player2.copy(busted = true),
            player3.copy(busted = true),
            player4.copy(blind = SmallBlind),
          ), 2, smallBlind)
          players.map(_.blind) shouldEqual List(SmallBlind, NoBlind, NoBlind, BigBlind)
          newButtonPosition shouldEqual 0
        }

        "BB jumping busted players (small blind went bust)" in {
          val (newButtonPosition, players) = nextDealerAndBlinds(List(
            player1.copy(blind = SmallBlind, busted = true),
            player2.copy(blind = BigBlind),
            player3.copy(busted = true),
            player4.copy(),
          ), 3, smallBlind)
          players.map(_.blind) shouldEqual List(NoBlind, SmallBlind, NoBlind, BigBlind)
          newButtonPosition shouldEqual 1
        }

        "BB jumping busted players (big blind went bust)" in {
          val (newButtonPosition, players) = nextDealerAndBlinds(List(
            player1.copy(blind = SmallBlind),
            player2.copy(blind = BigBlind, busted = true),
            player3.copy(busted = true),
            player4.copy(),
          ), 3, smallBlind)
          players.map(_.blind) shouldEqual List(NoBlind, NoBlind, NoBlind, BigBlind)
          newButtonPosition shouldEqual 0
        }
      }
    }

    "skipping busted players" - {
      "button skips a non-eligible (busted) player" in {
        val (newButtonIndex, _) = nextDealerAndBlinds(
          List(
            player1,
            player2.copy(busted = true),
            player3.copy(blind = SmallBlind),
            player4.copy(blind = BigBlind),
          ), 0, smallBlind
        )
        newButtonIndex shouldEqual 2
      }

      "small blind skips a non-eligible (busted) player" in {
        val (_, players) = nextDealerAndBlinds(
          List(
            player1,
            player2.copy(blind = SmallBlind),
            player3.copy(busted = true),
            player4.copy(blind = BigBlind),
          ), 0, smallBlind
        )
        players.map(_.blind) shouldEqual List(BigBlind, NoBlind, NoBlind, SmallBlind)
      }

      "big blind skips a non-eligible (busted) player" in {
        val (_, players) = nextDealerAndBlinds(
          List(
            player1,
            player2.copy(blind = SmallBlind),
            player3.copy(blind = BigBlind),
            player4.copy(busted = true),
          ), 0, smallBlind
        )
        players.map(_.blind) shouldEqual List(BigBlind, NoBlind, SmallBlind, NoBlind)
      }
    }

    "if only the big blind was present" - {
      "dealer stays" in {
        val (newButtonIndex, _) = nextDealerAndBlinds(
          List(
            player1,
            player2,
            player3.copy(blind = BigBlind),
            player4,
          ), 1, smallBlind
        )
        newButtonIndex shouldEqual 1
      }

      "BigBlind moves, prev Big is now Small" in {
        val (_, players) = nextDealerAndBlinds(
          List(
            player1,
            player2,
            player3.copy(blind = BigBlind),
            player4,
          ), 1, smallBlind
        )
        players.map(_.blind) shouldEqual List(NoBlind, NoBlind, SmallBlind, BigBlind)
      }
    }

    "if players are eliminated in this round it can be more complex" - {
      "if big and small blind were both present this round" - {
        "if the current small blind is busted" - {
          val players = List(
            player1,
            player2.copy(blind = SmallBlind, busted = true),
            player3.copy(blind = BigBlind),
            player4,
          )
          val button = 0

          "dealer stays" in {
            val (newButton, _) = nextDealerAndBlinds(players, button, smallBlind)
            newButton shouldEqual 0
          }

          "previous big blind is now small blind, next big blind is as normal" in {
            val (_, newPlayers) = nextDealerAndBlinds(players, button, smallBlind)
            newPlayers.map(_.blind) shouldEqual List(NoBlind, NoBlind, SmallBlind, BigBlind)
          }
        }

        "if the current big blind is busted" - {
          val players = List(
            player1,
            player2,
            player3.copy(blind = SmallBlind),
            player4.copy(blind = BigBlind, busted = true),
          )
          val button = 1

          "dealer moves" in {
            val (newButton, _) = nextDealerAndBlinds(players, button, smallBlind)
            newButton shouldEqual 2
          }

          "no small blind, next big blind as normal" in {
            val (_, newPlayers) = nextDealerAndBlinds(players, button, smallBlind)
            newPlayers.map(_.blind) shouldEqual List(BigBlind, NoBlind, NoBlind, NoBlind)
          }
        }

        "if both current blinds are busted" - {
          val players = List(
            player1.copy(blind = BigBlind, busted = true),
            player2,
            player3,
            player4,
            player5.copy(blind = SmallBlind, busted = true),
          )
          val button = 3

          "dealer stays" in {
            val (newButton, _) = nextDealerAndBlinds(players, button, smallBlind)
            newButton shouldEqual 3
          }

          "no small blind, next big blind as normal" in {
            val (_, newPlayers) = nextDealerAndBlinds(players, button, smallBlind)
            newPlayers.map(_.blind) shouldEqual List(NoBlind, BigBlind, NoBlind, NoBlind, NoBlind)
          }
        }

        "if both current blinds and the dealer are busted" - {
          val players = List(
            player1.copy(blind = SmallBlind, busted = true),
            player2.copy(blind = BigBlind, busted = true),
            player3,
            player4,
            player5,
            player6.copy(busted = true),
          )
          val button = 5

          "dealer moves back one" in {
            val (newButton, _) = nextDealerAndBlinds(players, button, smallBlind)
            newButton shouldEqual 4
          }

          "no small blind, next big blind as normal" in {
            val (_, newPlayers) = nextDealerAndBlinds(players, button, smallBlind)
            newPlayers.map(_.blind) shouldEqual List(NoBlind, NoBlind, BigBlind, NoBlind, NoBlind, NoBlind)
          }
        }
      }

      "if only the big blind was present" - {
        "if the current big blind is busted" - {
          val players = List(
            player1,
            player2.copy(busted = true),
            player3.copy(blind = BigBlind, busted = true),
            player4,
            player5,
          )
          val button = 0

          "dealer stays" in {
            val (newButton, _) = nextDealerAndBlinds(players, button, smallBlind)
            newButton shouldEqual 0
          }
          "no small blind again, next big blind as normal" in {
            val (_, newPlayers) = nextDealerAndBlinds(players, button, smallBlind)
            newPlayers.map(_.blind) shouldEqual List(NoBlind, NoBlind, NoBlind, BigBlind, NoBlind)
          }
        }

        "if both the current big blind and the dealer is busted" - {
          val players = List(
            player1.copy(busted = true),
            player2.copy(busted = true),
            player3.copy(blind = BigBlind, busted = true),
            player4,
            player5,
            player6,
          )
          val button = 0

          "dealer moves back one" in {
            val (newButton, _) = nextDealerAndBlinds(players, button, smallBlind)
            newButton shouldEqual 5
          }

          "no small blind again, next big blind as normal" in {
            val (_, newPlayers) = nextDealerAndBlinds(players, button, smallBlind)
            newPlayers.map(_.blind) shouldEqual List(NoBlind, NoBlind, NoBlind, BigBlind, NoBlind, NoBlind)
          }
        }
      }
    }

    "small blind amount is paid out" - {
      "player bets small blind amount" in {
        forAll(Gen.choose(1, 20)) { blind =>
          val (_, players) = nextDealerAndBlinds(
            List(
              player1.copy(stack = 1000),
              player2.copy(stack = 1000, blind = SmallBlind),
              player3.copy(stack = 1000, blind = BigBlind),
              player4.copy(stack = 1000),
            ), 0, blind)
          players.find(_.blind == SmallBlind).value.bet shouldEqual blind
        }
      }

      "player's stack is reduced by small blind amount" in {
        forAll(Gen.choose(1, 20)) { blind =>
          val (_, players) = nextDealerAndBlinds(
            List(
              player1.copy(stack = 1000),
              player2.copy(stack = 1000, blind = SmallBlind),
              player3.copy(stack = 1000, blind = BigBlind),
              player4.copy(stack = 1000),
            ), 0, blind)
          players.find(_.blind == SmallBlind).value.stack shouldEqual (1000 - blind)
        }
      }
    }

    "big blind amount is paid out" - {
      "player bets big blind amount" in {
        forAll(Gen.choose(1, 20)) { blind =>
          val (_, players) = nextDealerAndBlinds(
            List(
              player1.copy(stack = 1000),
              player2.copy(stack = 1000, blind = SmallBlind),
              player3.copy(stack = 1000, blind = BigBlind),
              player4.copy(stack = 1000),
            ), 0, blind)
          players.find(_.blind == BigBlind).value.bet shouldEqual (blind * 2)
        }
      }

      "player's stack is reduced by big blind amount" in {
        forAll(Gen.choose(1, 20)) { blind =>
          val (_, players) = nextDealerAndBlinds(
            List(
              player1.copy(stack = 1000),
              player2.copy(stack = 1000, blind = SmallBlind),
              player3.copy(stack = 1000, blind = BigBlind),
              player4.copy(stack = 1000),
            ), 0, blind)
          players.find(_.blind == BigBlind).value.stack shouldEqual (1000 - (2 * blind))
        }
      }
    }
  }

  "nextAliveAfterIndex" - {
    val gameId = GameId("game-id")
    val player1 = newPlayer(gameId, "player-1", false, PlayerAddress("player-address-1"), TestDates)
    val player2 = newPlayer(gameId, "player-2", false, PlayerAddress("player-address-2"), TestDates)
    val player3 = newPlayer(gameId, "player-3", false, PlayerAddress("player-address-3"), TestDates)

    "returns the other alive player with 2 players" - {
      "gets second from first" in {
        nextAliveAfterIndex(List(player1, player2), 0).value shouldEqual player2.playerId
      }

      "gets first from second" in {
        nextAliveAfterIndex(List(player1, player2), 1).value shouldEqual player1.playerId
      }

      "returns the only active player if the other player is busted" in {
        nextAliveAfterIndex(List(player1, player2.copy(busted = true)), 0).value shouldEqual player1.playerId
      }

      "returns None if both busted" in {
        val bustedPlayers = List(player1, player2).map(_.copy(busted = true))
        nextAliveAfterIndex(bustedPlayers, 0) shouldEqual None
      }
    }

    "returns the next player with 3 players" - {
      "gets second from first" in {
        nextAliveAfterIndex(List(player1, player2, player3), 0).value shouldEqual player2.playerId
      }

      "gets third from second" in {
        nextAliveAfterIndex(List(player1, player2, player3), 1).value shouldEqual player3.playerId
      }

      "gets first from third" in {
        nextAliveAfterIndex(List(player1, player2, player3), 2).value shouldEqual player1.playerId
      }

      "skips busted second from first" in {
        nextAliveAfterIndex(List(player1, player2.copy(busted = true), player3), 0).value shouldEqual player3.playerId
      }

      "skips busted third from second" in {
        nextAliveAfterIndex(List(player1, player2, player3.copy(busted = true)), 1).value shouldEqual player1.playerId
      }

      "returns None if all busted" in {
        val bustedPlayers = List(player1, player2, player3).map(_.copy(busted = true))
        nextAliveAfterIndex(bustedPlayers, 0) shouldEqual None
      }
    }

    "returns None if given an empty list" in {
      nextAliveAfterIndex(Nil, 0) shouldEqual None
    }
  }
}
