package io.adamnfish.pokerdot.logic

import io.adamnfish.pokerdot.{TestClock, TestHelpers}
import org.scalatest.freespec.AnyFreeSpec
import io.adamnfish.pokerdot.logic.Play._
import io.adamnfish.pokerdot.logic.Cards.RichRank
import io.adamnfish.pokerdot.logic.Games.newPlayer
import io.adamnfish.pokerdot.models.{Ace, BigBlind, BreakLevel, Clubs, Diamonds, Flop, GameId, Hole, NoBlind, Player, PlayerAddress, PlayerId, PreFlop, River, RoundLevel, Showdown, SmallBlind, Three, TimerStatus, Turn, Two}
import io.adamnfish.pokerdot.services.Clock
import org.scalacheck.Gen
import org.scalatest.{EitherValues, OptionValues}
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.util.Random


class PlayTest extends AnyFreeSpec with Matchers with ScalaCheckDrivenPropertyChecks with TestHelpers with OptionValues {
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
      newPlayer(gameId, "player-1", false, PlayerAddress("player-address-1"), TestClock),
      newPlayer(gameId, "player-2", false, PlayerAddress("player-address-2"), TestClock),
      newPlayer(gameId, "player-3", false, PlayerAddress("player-address-3"), TestClock),
      newPlayer(gameId, "player-4", false, PlayerAddress("player-address-4"), TestClock),
      newPlayer(gameId, "player-5", false, PlayerAddress("player-address-5"), TestClock),
      newPlayer(gameId, "player-6", false, PlayerAddress("player-address-6"), TestClock),
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
        val allPlayerCards = dealHoles(players, deckOrder(seed))
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
        val allPlayerCards = dealHoles(players, deckOrder(seed))
          .flatMap { player =>
            player.hole.toList
              .flatMap(h => List(h.card1, h.card2))
          }
        allPlayerCards shouldEqual allPlayerCards.distinct
      }
    }

    "deals empty holes to busted players (rather than excluding them from the result)" in {
      forAll { (bustedIndex: Int, seed: Long) =>
        val index = math.abs(bustedIndex % (players.size - 1))
        val playersWithBustedEntry = players.zipWithIndex.map {
          case (p, i) if i == index =>
            p.copy(busted = true)
          case (p, _) => p
        }
        val result = dealHoles(playersWithBustedEntry, deckOrder(seed))
        result.length shouldEqual players.length
        result(index).hole shouldEqual None
      }
    }
  }

  "lookupHoles" - {
    val player1 =
      Games.newPlayer(GameId("game-id"), "player-1", false, PlayerAddress("address-1"), TestClock)
        .copy(hole = Some(Hole(Ace of Clubs, Ace of Diamonds)))
    val player2 =
      Games.newPlayer(GameId("game-id"), "player-2", false, PlayerAddress("address-2"), TestClock)
        .copy(hole = Some(Hole(Two of Clubs, Two of Diamonds)))
    val player3 =
      Games.newPlayer(GameId("game-id"), "player-3", false, PlayerAddress("address-3"), TestClock)
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

  "playerIsActive" - {
    "true for an active player" in {
      val player = newPlayer(GameId("game-id"), "player-name", false, PlayerAddress("player-address"), TestClock)
      playerIsActive(player.copy(
        stack = 1000,
      )) shouldEqual true
    }

    "false for a folded player" in {
      val player = newPlayer(GameId("game-id"), "player-name", false, PlayerAddress("player-address"), TestClock)
      playerIsActive(player.copy(
        stack = 1000,
        folded = true,
      )) shouldEqual false
    }

    "false for a busted player" in {
      val player = newPlayer(GameId("game-id"), "player-name", false, PlayerAddress("player-address"), TestClock)
      playerIsActive(player.copy(
        stack = 1000,
        busted = true,
      )) shouldEqual false
    }

    "all-in players can no longer act, and are not active" in {
      val player = newPlayer(GameId("game-id"), "player-name", false, PlayerAddress("player-address"), TestClock)
      playerIsActive(player.copy(
        stack = 0,
      )) shouldEqual false
    }
  }

  "playerIsInvolved" - {
    "true for an active player" in {
      val player = newPlayer(GameId("game-id"), "player-name", false, PlayerAddress("player-address"), TestClock)
      playerIsInvolved(player.copy(
        stack = 1000,
        bet = 10,
        pot = 10,
      )) shouldEqual true
    }

    "an all-in player is still involved" in {
      val player = newPlayer(GameId("game-id"), "player-name", false, PlayerAddress("player-address"), TestClock)
      playerIsInvolved(player.copy(
        stack = 0,
        bet = 990,
        pot = 10,
      )) shouldEqual true
    }

    "folded players are not involved" in {
      val player = newPlayer(GameId("game-id"), "player-name", false, PlayerAddress("player-address"), TestClock)
      playerIsInvolved(player.copy(
        stack = 1000,
        bet = 10,
        pot = 10,
        folded = true,
      )) shouldEqual false
    }

    "busted players are not involved" in {
      val player = newPlayer(GameId("game-id"), "player-name", false, PlayerAddress("player-address"), TestClock)
      playerIsInvolved(player.copy(
        stack = 0,
        bet = 990,
        pot = 10,
        busted = true,
      )) shouldEqual false
    }
  }

  "playerIsYetToAct" - {
    val player =
      newPlayer(GameId("game-id"), "player-name", false, PlayerAddress("player-address"), TestClock)
        .copy(
          hole = Some(Hole(Ace of Clubs, Ace of Diamonds)),
          bet = 100,
          stack = 1000,
        )
    val otherPlayer =
      newPlayer(GameId("game-id"), "other-player-name", false, PlayerAddress("other-player-address"), TestClock)
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
          newPlayer(GameId("game-id"), "player-2", false, PlayerAddress("player-2-address"), TestClock)
            .copy(
              hole = Some(Hole(Ace of Clubs, Ace of Diamonds)),
              bet = 100,
              stack = 0,
            )
        val player3 =
          newPlayer(GameId("game-id"), "player-3", false, PlayerAddress("player-3-address"), TestClock)
            .copy(
              hole = Some(Hole(Ace of Clubs, Ace of Diamonds)),
              bet = 90,
              stack = 0,
            )
        val player4 =
          newPlayer(GameId("game-id"), "player-4", false, PlayerAddress("player-4-address"), TestClock)
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
          newPlayer(GameId("game-id"), "player-1", false, PlayerAddress("pa-1"), TestClock)
            .copy(bet = b1),
          newPlayer(GameId("game-id"), "player-2", false, PlayerAddress("pa-2"), TestClock)
            .copy(bet = b2),
          newPlayer(GameId("game-id"), "player-3", false, PlayerAddress("pa-3"), TestClock)
            .copy(bet = b3),
        )
        val result = currentBetAmount(players)
        result should (be >= b1 and be >= b2 and be >= b3)
      }
    }

    "excludes folded players from this calculation" in {
      val players = List(
        newPlayer(GameId("game-id"), "player-1", false, PlayerAddress("pa-1"), TestClock)
          .copy(bet = 10),
        newPlayer(GameId("game-id"), "player-2", false, PlayerAddress("pa-2"), TestClock)
          .copy(bet = 20),
        newPlayer(GameId("game-id"), "player-3", false, PlayerAddress("pa-3"), TestClock)
          .copy(
            bet = 30,
            folded = true,
          ),
      )
      currentBetAmount(players) shouldEqual 20
    }
  }

  "currentRaiseAmount" - {
    val player1 = newPlayer(GameId("game-id"), "player-1", false, PlayerAddress("pa-1"), TestClock)
    val player2 = newPlayer(GameId("game-id"), "player-2", false, PlayerAddress("pa-2"), TestClock)
    val player3 = newPlayer(GameId("game-id"), "player-3", false, PlayerAddress("pa-3"), TestClock)

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
    val p1 = newPlayer(GameId("game-id"), "p1", false, PlayerAddress("p1-address"), TestClock)
      .copy(stack = 1000, playerId = PlayerId("p1-id"))
    val p2 = newPlayer(GameId("game-id"), "p2", false, PlayerAddress("p2-address"), TestClock)
      .copy(stack = 1000, playerId = PlayerId("p2-id"))
    val p3 = newPlayer(GameId("game-id"), "p3", false, PlayerAddress("p3-address"), TestClock)
      .copy(stack = 1000, playerId = PlayerId("p3-id"))
    val p4 = newPlayer(GameId("game-id"), "p4", false, PlayerAddress("p4-address"), TestClock)
      .copy(stack = 1000, playerId = PlayerId("p4-id"))

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

      "if there is only one active player left" - {
        "returns None if the current player is the only player still in the round" in {
          nextPlayer(List(
            p1,
            p2.copy(folded = true),
            p3.copy(folded = true),
            p4.copy(folded = true),
          ), Some(p2.playerId), 0) shouldEqual None
        }

        "returns None if the current player is the only player still in the round - even if they have bet less" in {
          nextPlayer(List(
            p1.copy(bet = 5),
            p2.copy(bet = 10, folded = true),
            p3.copy(bet = 10, folded = true),
            p4.copy(folded = true),
          ), Some(p2.playerId), 0) shouldEqual None
        }

        "returns None after a heads-up fold (even if they have bet less)" in {
          nextPlayer(List(
            p1.copy(bet = 10, folded = true),
            p2.copy(bet = 5),
          ), Some(p1.playerId), 0) shouldEqual None
        }

        "returns the only active player if they need the chance to react to an all-in call" - {
          "with a single all-in player" in {
            nextPlayer(List(
              p1.copy(bet = 100),
              p2.copy(
                stack = 0,
                bet = p2.stack,
              ), // all-in
            ), Some(p1.playerId), 0) shouldEqual Some(p1.playerId)
          }

          "with multiple all-in players" in {
            nextPlayer(List(
              p1.copy(bet = 100),
              p2.copy(
                stack = 0,
                bet = p2.stack,
              ), // all-in
              p3.copy(
                stack = 0,
                bet = p3.stack,
              ), // all-in
            ), Some(p2.playerId), 0) shouldEqual Some(p1.playerId)
          }
        }

        "returns None if the only active player has already exceeded an all-in bet" in {
          nextPlayer(List(
            p1.copy(bet = 500), // already outbid the all-in player
            p2.copy(
              stack = 0,
              bet = 400,
            ), // all-in with small stack
          ), Some(p1.playerId), 0) shouldEqual None
        }

        "returns None if there is only one player still left in the game (everyone else is busted)" in {
          nextPlayer(List(
            p1.copy(bet = 5),
            p2.copy(busted = true),
            p3.copy(busted = true),
            p4.copy(busted = true),
          ), Some(p1.playerId), 0) shouldEqual None
        }
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

      "in heads-up, activates the next player" - {
        "with button index 0" in {
          nextPlayer(List(p1, p2), Some(p1.playerId), 0) shouldEqual Some(p2.playerId)
        }

        "with button index 1" in {
          nextPlayer(List(p1, p2), Some(p2.playerId), 1) shouldEqual Some(p1.playerId)
        }
      }

      "returns None if no players are eligible to become active" in {
        val ineligiblePlayers = List(p1, p2, p3, p4).map(_.copy(folded = true))
        nextPlayer(ineligiblePlayers, Some(p3.playerId), 0) shouldEqual None
      }

      "all players have acted" - {
        "returns None after a called bet (heads-up)" in {
          nextPlayer(List(
            p1.copy(
              blind = SmallBlind,
              bet = 25,
              checked = true,
            ),
            p2.copy(
              blind = BigBlind,
              bet = 25,
              checked = true,
            ),
          ), Some(p1.playerId), 0) shouldEqual None
        }

        "returns None after a called bet (larger game)" in {
          nextPlayer(List(
            p1.copy(
              bet = 25,
              checked = true,
            ),
            p2.copy(
              blind = SmallBlind,
              bet = 25,
              checked = true,
            ),
            p2.copy(
              blind = BigBlind,
              bet = 25,
              checked = true,
            ),
          ), Some(p1.playerId), 0) shouldEqual None
        }
      }
    }

    "when no player is currently active" - {
      "for the first phase of a round (with blinds paid)" - {
        "activates the player to the left of the Big Blind in a large game" - {
          "for button index 0" in {
            nextPlayer(List(
              p1,
              p2.copy(blind = SmallBlind, bet = 5),
              p3.copy(blind = BigBlind, bet = 10),
              p4,
            ), None, 0) shouldEqual Some(p4.playerId)
          }

          "for button index 1" in {
            nextPlayer(List(
              p1,
              p2,
              p3.copy(blind = SmallBlind, bet = 5),
              p4.copy(blind = BigBlind, bet = 10),
            ), None, 1) shouldEqual Some(p1.playerId)
          }

          "for button index 2" in {
            nextPlayer(List(
              p1.copy(blind = BigBlind, bet = 10),
              p2,
              p3,
              p4.copy(blind = SmallBlind, bet = 5),
            ), None, 2) shouldEqual Some(p2.playerId)
          }

          "wraps round to the first player for button index 3" in {
            nextPlayer(List(
              p1.copy(blind = SmallBlind, bet = 5),
              p2.copy(blind = BigBlind, bet = 10),
              p3,
              p4,
            ), None, 3) shouldEqual Some(p3.playerId)
          }
        }

        "in heads-up, activates the dealer / small blind (non big blind player)" - {
          "with button index 0" in {
            nextPlayer(List(
              p1.copy(blind = SmallBlind, bet = 5),
              p2.copy(blind = BigBlind, bet = 10),
            ), None, 0) shouldEqual Some(p1.playerId)
          }

          "with button index 1" in {
            nextPlayer(List(
              p1.copy(blind = BigBlind, bet = 10),
              p2.copy(blind = SmallBlind, bet = 5),
            ), None, 1) shouldEqual Some(p2.playerId)
          }
        }
      }

      "for a new phase after the first (no blinds)" - {
        "activates the player to the left of the button for a game of any size" - {
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

          "for heads up game with button index 0" in {
            nextPlayer(List(
              p1.copy(blind = BigBlind),
              p2.copy(blind = SmallBlind),
            ), None, 0) shouldEqual Some(p2.playerId)
          }

          "for heads up game with button index 1" in {
            nextPlayer(List(
              p1.copy(blind = SmallBlind),
              p2.copy(blind = BigBlind),
            ), None, 1) shouldEqual Some(p1.playerId)
          }
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
    val player1 = newPlayer(gameId, "player-1", false, PlayerAddress("player-address-1"), TestClock)
    val player2 = newPlayer(gameId, "player-2", false, PlayerAddress("player-address-2"), TestClock)
    val player3 = newPlayer(gameId, "player-3", false, PlayerAddress("player-address-3"), TestClock)
    val player4 = newPlayer(gameId, "player-4", false, PlayerAddress("player-address-4"), TestClock)
    val player5 = newPlayer(gameId, "player-5", false, PlayerAddress("player-address-5"), TestClock)
    val player6 = newPlayer(gameId, "player-6", false, PlayerAddress("player-address-6"), TestClock)
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
          val (newButton, players) = nextDealerAndBlinds(List(
            player1.copy(blind = BigBlind),
            player2.copy(blind = SmallBlind),
            player3.copy(busted = true),
            player4.copy(busted = true),
          ), 1, smallBlind)
          players.map(_.blind) shouldEqual List(SmallBlind, BigBlind, NoBlind, NoBlind)
          newButton shouldEqual 0
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

      "if small blind exceeds player's stack" - {
        "bet is limited to stack size" in {
          forAll(Gen.choose(5, 500)) { blind =>
            val smallBlindStack = math.max(0, blind - 5)
            val (_, players) = nextDealerAndBlinds(
              List(
                player1.copy(stack = 1000),
                player2.copy(stack = 1000, blind = SmallBlind),
                player3.copy(stack = smallBlindStack, blind = BigBlind), // next small blind
                player4.copy(stack = 1000),
              ), 0, blind)
            players.find(_.blind == SmallBlind).value.bet shouldEqual smallBlindStack
          }
        }

        "player's stack is empty (i.e. not negative)" in {
          forAll(Gen.choose(5, 500)) { blind =>
            val smallBlindStack = math.max(0, blind - 5)
            val (_, players) = nextDealerAndBlinds(
              List(
                player1.copy(stack = 1000),
                player2.copy(stack = 1000, blind = SmallBlind),
                player3.copy(stack = smallBlindStack, blind = BigBlind), // next small blind
                player4.copy(stack = 1000),
              ), 0, blind)
            players.find(_.blind == SmallBlind).value.stack shouldEqual 0
          }
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

      "if small blind exceeds player's stack" - {
        "bet is limited to stack size" in {
          forAll(Gen.choose(5, 500)) { blind =>
            val bigBlindStack = math.max(0, (2 * blind) - 5)
            val (_, players) = nextDealerAndBlinds(
              List(
                player1.copy(stack = 1000),
                player2.copy(stack = 1000, blind = SmallBlind),
                player3.copy(stack = 1000, blind = BigBlind),
                player4.copy(stack = bigBlindStack), // next big blind
              ), 0, blind)
            players.find(_.blind == BigBlind).value.bet shouldEqual bigBlindStack
          }
        }

        "player's stack is empty (i.e. not negative)" in {
          forAll(Gen.choose(5, 500)) { blind =>
            val bigBlindStack = math.max(0, (2 * blind) - 5)
            val (_, players) = nextDealerAndBlinds(
              List(
                player1.copy(stack = 1000),
                player2.copy(stack = 1000, blind = SmallBlind),
                player3.copy(stack = 1000, blind = BigBlind),
                player4.copy(stack = bigBlindStack), // next big blind
              ), 0, blind)
            players.find(_.blind == BigBlind).value.stack shouldEqual 0
          }
        }
      }
    }

    "if there is only one active player (game is over)" - {
      val gameId = GameId("game-id")
      val players = List(
        newPlayer(gameId, "player-1", false, PlayerAddress("player-address-1"), TestClock)
          .copy(busted = true),
        newPlayer(gameId, "player-2", false, PlayerAddress("player-address-2"), TestClock)
          .copy(busted = true, blind = SmallBlind),
        newPlayer(gameId, "player-3", false, PlayerAddress("player-address-3"), TestClock)
          .copy(blind = BigBlind),
      ).map(_.copy(busted = true))
      val smallBlind = 5

      "dealer does not move" in {
        val (newButton, _) = nextDealerAndBlinds(players, 0, smallBlind)
        newButton shouldEqual 0
      }

      "no players have any blinds" in {
        val (_, newPlayers) = nextDealerAndBlinds(players, 0, smallBlind)
        newPlayers.map(_.blind) should contain only NoBlind
      }
    }
  }

  "blindForNextRound" - {
    "returns the current small blind if no timer status is present" in {
      forAll { sb: Int =>
        blindForNextRound(sb, 0, None) shouldEqual Right(sb)
      }
    }

    "if timer is playing" - {
      "if we're at the correct value, blind remains unchanged" in {
        val currentSmallBlind = 10
        val timerStatus = TimerStatus(0, None, List(
          RoundLevel(300, 10),
        ))
        val newSmallBlind = blindForNextRound(currentSmallBlind, 100 * 1000, Some(timerStatus)).value
        newSmallBlind shouldEqual currentSmallBlind
      }

      "if there has been a timer level advancement, the blinds increase as directed by the timer level" in {
        val currentSmallBlind = 10
        val timerStatus = TimerStatus(0, None, List(
          RoundLevel(100, 10),
          RoundLevel(100, 20),
        ))
        val newSmallBlind = blindForNextRound(currentSmallBlind, 120 * 1000, Some(timerStatus)).value
        newSmallBlind shouldEqual 20
      }

      "if multiple timer levels have passed, we update to the most recent (the one that is correct right now)" in {
        val currentSmallBlind = 10
        val timerStatus = TimerStatus(0, None, List(
          RoundLevel(100, 10),
          RoundLevel(100, 20),
          RoundLevel(100, 40),
          RoundLevel(100, 80),
        ))
        val newSmallBlind = blindForNextRound(currentSmallBlind, 250 * 1000, Some(timerStatus)).value
        newSmallBlind shouldEqual 40
      }

      "if we drop off the end of the timer" - {
        "take the most recent level as the blind value" in {
          val currentSmallBlind = 10
          val timerStatus = TimerStatus(0, None, List(
            RoundLevel(100, 10),
            RoundLevel(100, 20),
            RoundLevel(100, 50),
          ))
          val newSmallBlind = blindForNextRound(currentSmallBlind, 500 * 1000, Some(timerStatus)).value
          newSmallBlind shouldEqual 50
        }

        "ignore any trailing breaks to find the last valid timer level" in {
          val currentSmallBlind = 10
          val timerStatus = TimerStatus(0, None, List(
            RoundLevel(100, 10),
            RoundLevel(100, 20),
            RoundLevel(100, 50),
            BreakLevel(100),
          ))
          val newSmallBlind = blindForNextRound(currentSmallBlind, 500 * 1000, Some(timerStatus)).value
          newSmallBlind shouldEqual 50
        }
      }

      "fails to advance the round if we're on a break" in {
        val currentSmallBlind = 10
        val timerStatus = TimerStatus(0, None, List(
          RoundLevel(100, 10),
          RoundLevel(100, 20),
          BreakLevel(100),
          RoundLevel(100, 50),
        ))
        val result = blindForNextRound(currentSmallBlind, 250 * 1000, Some(timerStatus))
        result.isLeft shouldEqual true

      }
    }

    "fails to advance the round if timer is paused" in {
      val currentSmallBlind = 10
      val timerStatus = TimerStatus(0, Some(80 * 1000), List(
        RoundLevel(100, 10),
        RoundLevel(100, 20),
        BreakLevel(100),
        RoundLevel(100, 50),
      ))
      val result = blindForNextRound(currentSmallBlind, 120 * 1000, Some(timerStatus))
      result.isLeft shouldEqual true
    }
  }

  "timerSmallBlind" - {
    // most cases are covered above in blindForNextRound

    "calculates the correct blind amount for a running timer" in {
      val timerStatus = TimerStatus(0, None, List(
        RoundLevel(100, 10),
        RoundLevel(100, 20),
        BreakLevel(100),
        RoundLevel(100, 50),
      ))
      timerSmallBlind(timerStatus, 150 * 1000) shouldEqual Right((20, false))
    }

    "calculates the correct blind amount for a running timer that started after 0" in {
      val timerStatus = TimerStatus(100000 * 1000, None, List(
        RoundLevel(100, 10),
        RoundLevel(100, 20),
        BreakLevel(100),
        RoundLevel(100, 50),
      ))
      timerSmallBlind(timerStatus, (100000 + 150) * 1000) shouldEqual Right((20, false))
    }

    "takes the last blind amount for an expired timer" in {
      val timerStatus = TimerStatus(0, None, List(
        RoundLevel(100, 10),
        RoundLevel(100, 20),
        BreakLevel(100),
        RoundLevel(100, 50),
      ))
      timerSmallBlind(timerStatus, 600 * 1000) shouldEqual Right((50, false))
    }

    "calculates the correct blind amount if the timer is paused" in {
      val timerStatus = TimerStatus(0, Some(80 * 1000), List(
        RoundLevel(100, 10),
        RoundLevel(100, 20),
        BreakLevel(100),
        RoundLevel(100, 50),
      ))
      timerSmallBlind(timerStatus, 800 * 1000) shouldEqual Right((10, false))
    }

    "takes the last valid blind amount if we're on a break" in {
      val timerStatus = TimerStatus(0, None, List(
        RoundLevel(100, 10),
        RoundLevel(100, 20),
        BreakLevel(100),
        RoundLevel(100, 50),
      ))
      timerSmallBlind(timerStatus, 250 * 1000) shouldEqual Right((20, true))
    }

    "takes the last valid blind amount if we're paused during a break" in {
      val timerStatus = TimerStatus(0, Some(250 * 1000), List(
        RoundLevel(100, 10),
        RoundLevel(100, 20),
        BreakLevel(100),
        RoundLevel(100, 50),
      ))
      timerSmallBlind(timerStatus, 1000 * 1000) shouldEqual Right((20, true))
    }
  }

  "nextAliveAfterIndex" - {
    val gameId = GameId("game-id")
    val player1 = newPlayer(gameId, "player-1", false, PlayerAddress("player-address-1"), TestClock)
    val player2 = newPlayer(gameId, "player-2", false, PlayerAddress("player-address-2"), TestClock)
    val player3 = newPlayer(gameId, "player-3", false, PlayerAddress("player-address-3"), TestClock)

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
