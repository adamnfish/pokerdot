package io.adamnfish.pokerdot.logic

import io.adamnfish.pokerdot.{PokerGenerators, TestDates, TestHelpers}
import io.adamnfish.pokerdot.logic.Cards.RichRank
import io.adamnfish.pokerdot.logic.Games.newPlayer
import io.adamnfish.pokerdot.logic.PokerHands.{bestHand, bestHands, cardOrd, findDuplicateRanks, findDuplicateSuits, flush, fourOfAKind, fullHouse, handOrd, highCard, pair, playerWinnings, rankOrd, straight, straightFlush, suitOrd, threeOfAKind, twoPair, winnings}
import io.adamnfish.pokerdot.models._
import io.adamnfish.pokerdot.utils.IntHelpers.abs
import org.scalacheck.Gen
import org.scalatest.OptionValues
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.util.Random


class PokerHandsTest extends AnyFreeSpec with Matchers with ScalaCheckDrivenPropertyChecks with TestHelpers with OptionValues with PokerGenerators {
  "bestHand" - {
    "returns 'high card' when nothing better exists" - {
      "with 7 cards" in {
        forAll(nothingConnectsCardsGen(7)) {
          case card1 :: card2 :: card3 :: card4 :: card5 :: card6 :: card7 :: Nil =>
            val hand = bestHand(card1, card2, card3, card4, card5, Some(card6), Some(card7))
            hand shouldBe a[HighCard]
          case _ =>
            fail("generator did not provide 7 cards")
        }
      }
      "with 6 cards" in {
        forAll(nothingConnectsCardsGen(6)) {
          case card1 :: card2 :: card3 :: card4 :: card5 :: card6 :: Nil =>
            val hand = bestHand(card1, card2, card3, card4, card5, Some(card6), None)
            hand shouldBe a[HighCard]
          case _ =>
            fail("generator did not provide 6 cards")
        }
      }
      "with 5 cards" in {
        forAll(nothingConnectsCardsGen(5)) {
          case card1 :: card2 :: card3 :: card4 :: card5 :: Nil =>
            val hand = bestHand(card1, card2, card3, card4, card5, None, None)
            hand shouldBe a[HighCard]
          case _ =>
            fail("generator did not provide 5 cards")
        }
      }
    }

    "returns 'pair' for cards containing a pair" - {
      "with 7 cards" in {
        forAll(pairCardsGen(7)) {
          case card1 :: card2 :: card3 :: card4 :: card5 :: card6 :: card7 :: Nil =>
            val hand = bestHand(card1, card2, card3, card4, card5, Some(card6), Some(card7))
            hand shouldBe a[Pair]
          case _ =>
            fail("generator did not provide 7 cards")
        }
      }
      "with 6 cards" in {
        forAll(pairCardsGen(6)) {
          case card1 :: card2 :: card3 :: card4 :: card5 :: card6 :: Nil =>
            val hand = bestHand(card1, card2, card3, card4, card5, Some(card6), None)
            hand shouldBe a[Pair]
          case _ =>
            fail("generator did not provide 6 cards")
        }
      }
      "with 5 cards" in {
        forAll(pairCardsGen(5)) {
          case card1 :: card2 :: card3 :: card4 :: card5 :: Nil =>
            val hand = bestHand(card1, card2, card3, card4, card5, None, None)
            hand shouldBe a[Pair]
          case _ =>
            fail("generator did not provide 6 cards")
        }
      }
    }

    "returns 'two pair' for cards containing two pairs" - {
      "with 7 cards" in {
        forAll(twoPairCardsGen(7)) {
          case card1 :: card2 :: card3 :: card4 :: card5 :: card6 :: card7 :: Nil =>
            val hand = bestHand(card1, card2, card3, card4, card5, Some(card6), Some(card7))
            hand shouldBe a[TwoPair]
          case _ =>
            fail("generator did not provide 7 cards")
        }
      }
      "with 6 cards" in {
        forAll(twoPairCardsGen(6)) {
          case card1 :: card2 :: card3 :: card4 :: card5 :: card6 :: Nil =>
            val hand = bestHand(card1, card2, card3, card4, card5, Some(card6), None)
            hand shouldBe a[TwoPair]
          case _ =>
            fail("generator did not provide 6 cards")
        }
      }
      "with 5 cards" in {
        forAll(twoPairCardsGen(5)) {
          case card1 :: card2 :: card3 :: card4 :: card5 :: Nil =>
            val hand = bestHand(card1, card2, card3, card4, card5, None, None)
            hand shouldBe a[TwoPair]
          case _ =>
            fail("generator did not provide 5 cards")
        }
      }
    }

    "returns 'three of a kind' for cards containing a trip" - {
      "with 7 cards" in {
        forAll(threeOfAKindCardsGen(7)) {
          case card1 :: card2 :: card3 :: card4 :: card5 :: card6 :: card7 :: Nil =>
            val hand = bestHand(card1, card2, card3, card4, card5, Some(card6), Some(card7))
            hand shouldBe a[ThreeOfAKind]
          case _ =>
            fail("generator did not provide 7 cards")
        }
      }
      "with 6 cards" in {
        forAll(threeOfAKindCardsGen(6)) {
          case card1 :: card2 :: card3 :: card4 :: card5 :: card6 :: Nil =>
            val hand = bestHand(card1, card2, card3, card4, card5, Some(card6), None)
            hand shouldBe a[ThreeOfAKind]
          case _ =>
            fail("generator did not provide 6 cards")
        }
      }
      "with 5 cards" in {
        forAll(threeOfAKindCardsGen(5)) {
          case card1 :: card2 :: card3 :: card4 :: card5 :: Nil =>
            val hand = bestHand(card1, card2, card3, card4, card5, None, None)
            hand shouldBe a[ThreeOfAKind]
          case _ =>
            fail("generator did not provide 5 cards")
        }
      }
    }

    "returns 'straight' for cards containing a straight" - {
      "with 7 cards" in {
        forAll(straightCardsGen(7)) {
          case card1 :: card2 :: card3 :: card4 :: card5 :: card6 :: card7 :: Nil =>
            val hand = bestHand(card1, card2, card3, card4, card5, Some(card6), Some(card7))
            hand shouldBe a[Straight]
          case _ =>
            fail("generator did not provide 7 cards")
        }
      }
      "with 6 cards" in {
        forAll(straightCardsGen(6)) {
          case card1 :: card2 :: card3 :: card4 :: card5 :: card6 :: Nil =>
            val hand = bestHand(card1, card2, card3, card4, card5, Some(card6), None)
            hand shouldBe a[Straight]
          case _ =>
            fail("generator did not provide 6 cards")
        }
      }
      "with 5 cards" in {
        forAll(straightCardsGen(5)) {
          case card1 :: card2 :: card3 :: card4 :: card5 :: Nil =>
            val hand = bestHand(card1, card2, card3, card4, card5, None, None)
            hand shouldBe a[Straight]
          case _ =>
            fail("generator did not provide 5 cards")
        }
      }
    }

    "returns 'flush' for cards containing a flush" - {
      "with 7 cards" in {
        forAll(flushCardsGen(7)) {
          case card1 :: card2 :: card3 :: card4 :: card5 :: card6 :: card7 :: Nil =>
            val hand = bestHand(card1, card2, card3, card4, card5, Some(card6), Some(card7))
            hand shouldBe a[Flush]
          case _ =>
            fail("generator did not provide 7 cards")
        }
      }
      "with 6 cards" in {
        forAll(flushCardsGen(6)) {
          case card1 :: card2 :: card3 :: card4 :: card5 :: card6 :: Nil =>
            val hand = bestHand(card1, card2, card3, card4, card5, Some(card6), None)
            hand shouldBe a[Flush]
          case _ =>
            fail("generator did not provide 6 cards")
        }
      }
      "with 5 cards" in {
        forAll(flushCardsGen(5)) {
          case card1 :: card2 :: card3 :: card4 :: card5 :: Nil =>
            val hand = bestHand(card1, card2, card3, card4, card5, None, None)
            hand shouldBe a[Flush]
          case _ =>
            fail("generator did not provide 5 cards")
        }
      }
    }

    "returns 'full house' for cards containing a full house" - {
      "with 7 cards" in {
        forAll(fullHouseCardsGen(7)) {
          case card1 :: card2 :: card3 :: card4 :: card5 :: card6 :: card7 :: Nil =>
            val hand = bestHand(card1, card2, card3, card4, card5, Some(card6), Some(card7))
            hand shouldBe a[FullHouse]
          case _ =>
            fail("generator did not provide 7 cards")
        }
      }
      "with 6 cards" in {
        forAll(fullHouseCardsGen(6)) {
          case card1 :: card2 :: card3 :: card4 :: card5 :: card6 :: Nil =>
            val hand = bestHand(card1, card2, card3, card4, card5, Some(card6), None)
            hand shouldBe a[FullHouse]
          case _ =>
            fail("generator did not provide 6 cards")
        }
      }
      "with 5 cards" in {
        forAll(fullHouseCardsGen(5)) {
          case card1 :: card2 :: card3 :: card4 :: card5 :: Nil =>
            val hand = bestHand(card1, card2, card3, card4, card5, None, None)
            hand shouldBe a[FullHouse]
          case _ =>
            fail("generator did not provide 5 cards")
        }
      }
    }

    "returns 'four of a kind' for cards containing a quad" - {
      "with 7 cards" in {
        forAll(fourOfAKindCardsGen(7)) {
          case card1 :: card2 :: card3 :: card4 :: card5 :: card6 :: card7 :: Nil =>
            val hand = bestHand(card1, card2, card3, card4, card5, Some(card6), Some(card7))
            hand shouldBe a[FourOfAKind]
          case _ =>
            fail("generator did not provide 7 cards")
        }
      }
      "with 6 cards" in {
        forAll(fourOfAKindCardsGen(6)) {
          case card1 :: card2 :: card3 :: card4 :: card5 :: card6 :: Nil =>
            val hand = bestHand(card1, card2, card3, card4, card5, Some(card6), None)
            hand shouldBe a[FourOfAKind]
          case _ =>
            fail("generator did not provide 6 cards")
        }
      }
      "with 5 cards" in {
        forAll(fourOfAKindCardsGen(5)) {
          case card1 :: card2 :: card3 :: card4 :: card5 :: Nil =>
            val hand = bestHand(card1, card2, card3, card4, card5, None, None)
            hand shouldBe a[FourOfAKind]
          case _ =>
            fail("generator did not provide 5 cards")
        }
      }
    }

    "returns 'straight flush' for cards containing a straight flush" - {
      "with 7 cards" in {
        forAll(straightFlushCardsGen(7)) {
          case card1 :: card2 :: card3 :: card4 :: card5 :: card6 :: card7 :: Nil =>
            val hand = bestHand(card1, card2, card3, card4, card5, Some(card6), Some(card7))
            hand shouldBe a[StraightFlush]
          case _ =>
            fail("generator did not provide 7 cards")
        }
      }
      "with 6 cards" in {
        forAll(straightFlushCardsGen(6)) {
          case card1 :: card2 :: card3 :: card4 :: card5 :: card6 :: Nil =>
            val hand = bestHand(card1, card2, card3, card4, card5, Some(card6), None)
            hand shouldBe a[StraightFlush]
          case _ =>
            fail("generator did not provide 6 cards")
        }
      }
      "with 5 cards" in {
        forAll(straightFlushCardsGen(5)) {
          case card1 :: card2 :: card3 :: card4 :: card5 :: Nil =>
            val hand = bestHand(card1, card2, card3, card4, card5, None, None)
            hand shouldBe a[StraightFlush]
          case _ =>
            fail("generator did not provide 5 cards")
        }
      }
    }
  }

  "bestHands" - {
    "returns the same hand as `bestHand` for each player" - {
      "with all 7 cards" in {
        forAll(nothingConnectsCardsGen()) {
          case f1 :: f2 :: f3 :: h1 :: h2 :: t :: r :: Nil =>
            val round = Round(
              Showdown, 0,
              Two of Clubs, f1, f2, f3, Two of Spades, t, Two of Diamonds, r
            )
            val players = (1 to 10).toList.map { i =>
              newPlayer(GameId("game-id"), s"player-$i", false, PlayerAddress(s"pa-$i"), TestDates)
                .copy(hole = Some(Hole(h1, h2)))
            }
            val playerHands = bestHands(round, players)
            val expectedBestHand = bestHand(f1, f2, f3, h1, h2, Some(t), Some(r))
            playerHands shouldEqual players.map(p => PlayerHand(p, expectedBestHand))
          case _ =>
            fail("incorrect card generation")
        }
      }

      "with 5 cards after the flop" in {
        forAll(nothingConnectsCardsGen()) {
          case f1 :: f2 :: f3 :: h1 :: h2 :: t :: r :: Nil =>
            val round = Round(
              Flop, 0,
              Two of Clubs, f1, f2, f3, Two of Spades, t, Two of Diamonds, r
            )
            val players = (1 to 10).toList.map { i =>
              newPlayer(GameId("game-id"), s"player-$i", false, PlayerAddress(s"pa-$i"), TestDates)
                .copy(hole = Some(Hole(h1, h2)))
            }
            val playerHands = bestHands(round, players)
            val expectedBestHand = bestHand(f1, f2, f3, h1, h2, None, None)
            playerHands shouldEqual players.map(p => PlayerHand(p, expectedBestHand))
          case _ =>
            fail("incorrect card generation")
        }
      }

      "with 6 cards after the turn" in {
        forAll(nothingConnectsCardsGen()) {
          case f1 :: f2 :: f3 :: h1 :: h2 :: t :: r :: Nil =>
            val round = Round(
              Turn, 0,
              Two of Clubs, f1, f2, f3, Two of Spades, t, Two of Diamonds, r
            )
            val players = (1 to 10).toList.map { i =>
              newPlayer(GameId("game-id"), s"player-$i", false, PlayerAddress(s"pa-$i"), TestDates)
                .copy(hole = Some(Hole(h1, h2)))
            }
            val playerHands = bestHands(round, players)
            val expectedBestHand = bestHand(f1, f2, f3, h1, h2, Some(t), Some(r))
            playerHands shouldEqual players.map(p => PlayerHand(p, expectedBestHand))
          case _ =>
            fail("incorrect card generation")
        }
      }
    }

    "TODO - different hands example as well" ignore {}
  }

  "winnings" - {
    // The simple cases handle the vast majority of games.
    // The edge cases get box-of-birds-mad, and need to be handled as well.
    // There are some detailed comments in the code

    val gameId = GameId("game-id")

    def testPlayerHand(pot: Int, card1: Card, card2: Card, id: String, folded: Boolean = false): PlayerHand = {
      PlayerHand(
        Player(
          gameId, PlayerId(s"player-$id"), 0, PlayerAddress(s"player-$id-address"), PlayerKey(s"$id"), s"Player $id", 1000,
          pot = pot, 0, false,
          folded = folded, false,
          hole = Some(Hole(card1, card2)), true, false, false, NoBlind
        ),
        bestHand(card1, card2,
          // community cards
          King of Clubs, Queen of Diamonds, Jack of Spades, Some(Nine of Clubs), Some(Seven of Hearts)
        )
      )
    }

    "for simple cases" - {
      "two players involved, clear winner" in {
        val playerPair = testPlayerHand(50, King of Spades, Four of Hearts, "1")
        val playerHighCard = testPlayerHand(50, Five of Spades, Four of Clubs, "2")

        winnings(List(playerPair, playerHighCard)) shouldEqual List(
          PotWinnings(
            potSize = 100,
            participants = Set(playerPair.player.playerId, playerHighCard.player.playerId),
            winners = Set(playerPair.player.playerId)
          )
        )
      }

      "three players involved, clear winner" in {
        val playerHighCard = testPlayerHand(50, Five of Spades, Four of Clubs, "1")
        val playerPair = testPlayerHand(50, King of Spades, Four of Hearts, "2")
        val playerTrips = testPlayerHand(50, Seven of Clubs, Seven of Hearts, "3")

        winnings(List(playerPair, playerHighCard, playerTrips)) shouldEqual List(
          PotWinnings(
            potSize = 150,
            participants = Set(playerHighCard.player.playerId, playerPair.player.playerId, playerTrips.player.playerId),
            winners = Set(playerTrips.player.playerId)
          )
        )
      }

      "a player folded" in {
        val playerHighCard = testPlayerHand(50, Five of Spades, Four of Clubs, "1")
        val playerPair = testPlayerHand(50, King of Spades, Four of Hearts, "2")
        val playerTrips = testPlayerHand(50, Seven of Clubs, Seven of Hearts, "3")
        val foldedPlayer = testPlayerHand(25, Two of Hearts, Four of Diamonds, "4", folded = true)

        winnings(List(playerPair, playerHighCard, playerTrips, foldedPlayer)) shouldEqual List(
          PotWinnings(
            potSize = 175,
            participants = Set(playerHighCard.player.playerId, playerPair.player.playerId, playerTrips.player.playerId),
            winners = Set(playerTrips.player.playerId)
          )
        )
      }

      "multiple players folded" in {
        val playerHighCard = testPlayerHand(50, Five of Spades, Four of Clubs, "1")
        val playerPair = testPlayerHand(50, King of Spades, Four of Hearts, "2")
        val foldedPlayer = testPlayerHand(25, Two of Hearts, Four of Diamonds, "4", folded = true)
        val foldedPlayer2 = testPlayerHand(25, Two of Spades, Four of Clubs, "4", folded = true)

        winnings(List(playerPair, playerHighCard, foldedPlayer, foldedPlayer2)) shouldEqual List(
          PotWinnings(
            potSize = 150,
            participants = Set(playerHighCard.player.playerId, playerPair.player.playerId),
            winners = Set(playerPair.player.playerId)
          )
        )
      }

      "only one player remains" in {
        val playerHighCard = testPlayerHand(50, Five of Spades, Four of Clubs, "1")
        val foldedPlayer1 = testPlayerHand(50, Three of Spades, Four of Hearts, "2", folded = true)
        val foldedPlayer2 = testPlayerHand(25, Two of Hearts, Four of Diamonds, "4", folded = true)
        val foldedPlayer3 = testPlayerHand(25, Two of Spades, Four of Clubs, "4", folded = true)

        winnings(List(playerHighCard, foldedPlayer1, foldedPlayer2, foldedPlayer3)) shouldEqual List(
          PotWinnings(
            potSize = 150,
            participants = Set(playerHighCard.player.playerId),
            winners = Set(playerHighCard.player.playerId)
          )
        )
      }

      "if player with strongest hand folds, they do not win" in {
        val playerHighCard = testPlayerHand(50, Five of Spades, Four of Clubs, "1")
        val playerPair = testPlayerHand(50, King of Spades, Four of Hearts, "2")
        val foldedPlayerTrips = testPlayerHand(25, Seven of Clubs, Seven of Hearts, "3", folded = true)

        winnings(List(playerPair, playerHighCard, foldedPlayerTrips)) shouldEqual List(
          PotWinnings(
            potSize = 125,
            participants = Set(playerHighCard.player.playerId, playerPair.player.playerId),
            winners = Set(playerPair.player.playerId)
          )
        )
      }

      "heads up, player folds" in {
        val playerBlind = testPlayerHand(1, King of Spades, Four of Hearts, "1")
        val playerFold = testPlayerHand(0, Five of Spades, Four of Clubs, "2", folded = true)

        winnings(List(playerBlind, playerFold)) shouldEqual List(
          PotWinnings(
            potSize = 1,
            participants = Set(playerBlind.player.playerId),
            winners = Set(playerBlind.player.playerId)
          )
        )
      }

      "heads up, stronger player folds - should only include winning pot" in {
        val playerBlind = testPlayerHand(10, Five of Spades, Two of Clubs, "1")
        val playerFold = testPlayerHand(5, Ace of Spades, Ten of Hearts, "2", folded = true)

        winnings(List(playerBlind, playerFold)) shouldEqual List(
          PotWinnings(
            potSize = 15,
            participants = Set(playerBlind.player.playerId),
            winners = Set(playerBlind.player.playerId)
          )
        )
      }
    }

    "for split pots" - {
      "two players split a pot" in {
        val playerHighCard = testPlayerHand(50, Five of Spades, Four of Clubs, "1")
        val playerPair1 = testPlayerHand(50, King of Spades, Four of Hearts, "2")
        val playerPair2 = testPlayerHand(50, King of Diamonds, Four of Spades, "3")

        winnings(List(playerHighCard, playerPair1, playerPair2)) shouldEqual List(
          PotWinnings(
            potSize = 150,
            participants = Set(playerHighCard.player.playerId, playerPair1.player.playerId, playerPair2.player.playerId),
            winners = Set(playerPair1.player.playerId, playerPair2.player.playerId)
          )
        )
      }

      "three players split a pot" in {
        val playerHighCard = testPlayerHand(50, Five of Spades, Three of Clubs, "1")
        val playerPair1 = testPlayerHand(50, King of Spades, Four of Hearts, "2")
        val playerPair2 = testPlayerHand(50, King of Diamonds, Four of Spades, "3")
        val playerPair3 = testPlayerHand(50, King of Hearts, Four of Clubs, "4")

        winnings(List(playerHighCard, playerPair1, playerPair2, playerPair3)) shouldEqual List(
          PotWinnings(
            potSize = 200,
            participants = Set(playerHighCard.player.playerId, playerPair1.player.playerId, playerPair2.player.playerId, playerPair3.player.playerId),
            winners = Set(playerPair1.player.playerId, playerPair2.player.playerId, playerPair3.player.playerId)
          )
        )
      }
    }

    "when a player is all-in (side-pots)" - {
      "winning player was all-in, second place gets the balance" in {
        val playerHighCard = testPlayerHand(50, Five of Spades, Four of Clubs, "1")
        val playerPair = testPlayerHand(50, King of Spades, Four of Hearts, "2")
        val playerTrips = testPlayerHand(10, Seven of Clubs, Seven of Hearts, "3")

        winnings(List(playerHighCard, playerPair, playerTrips)) shouldEqual List(
          PotWinnings(
            potSize = 80,
            participants = Set(playerHighCard.player.playerId, playerPair.player.playerId),
            winners = Set(playerPair.player.playerId),
          ),
          PotWinnings(
            potSize = 30,
            participants = Set(playerHighCard.player.playerId, playerPair.player.playerId, playerTrips.player.playerId),
            winners = Set(playerTrips.player.playerId),
          ),
        )
      }

      "all-in player loses, another player can win whole balance" in {
        val playerHighCard = testPlayerHand(10, Five of Spades, Four of Clubs, "1")
        val playerPair = testPlayerHand(50, King of Spades, Four of Hearts, "2")
        val playerTrips = testPlayerHand(50, Seven of Clubs, Seven of Hearts, "3")

        winnings(List(playerHighCard, playerPair, playerTrips)) shouldEqual List(
          PotWinnings(
            potSize = 80,
            participants = Set(playerPair.player.playerId, playerTrips.player.playerId),
            winners = Set(playerTrips.player.playerId),
          ),
          PotWinnings(
            potSize = 30,
            participants = Set(playerHighCard.player.playerId, playerPair.player.playerId, playerTrips.player.playerId),
            winners = Set(playerTrips.player.playerId),
          ),
        )
      }

      "side-pot gets split between 2 players, balance goes to a lesser winner" in {
        val playerHighCard = testPlayerHand(50, Five of Spades, Four of Clubs, "1")
        val playerHigherCard = testPlayerHand(50, Ace of Spades, Four of Clubs, "2")
        val playerPair1 = testPlayerHand(10, King of Spades, Four of Hearts, "3")
        val playerPair2 = testPlayerHand(10, King of Diamonds, Four of Spades, "4")

        winnings(List(playerHighCard, playerHigherCard, playerPair1, playerPair2)) shouldEqual List(
          PotWinnings(
            potSize = 80,
            participants = Set(playerHighCard.player.playerId, playerHigherCard.player.playerId),
            winners = Set(playerHigherCard.player.playerId),
          ),
          PotWinnings(
            potSize = 40,
            participants = Set(playerHighCard.player.playerId, playerHigherCard.player.playerId, playerPair1.player.playerId, playerPair2.player.playerId),
            winners = Set(playerPair1.player.playerId, playerPair2.player.playerId),
          ),
        )
      }

      "side-pot gets split between 2 players, balance is split between 2 tied lesser players" in {
        val playerHighCard1 = testPlayerHand(50, Ace of Spades, Four of Diamonds, "1")
        val playerHighCard2 = testPlayerHand(50, Ace of Diamonds, Four of Clubs, "2")
        val playerPair1 = testPlayerHand(20, King of Spades, Four of Hearts, "3")
        val playerPair2 = testPlayerHand(20, King of Diamonds, Four of Spades, "4")

        winnings(List(playerHighCard1, playerHighCard2, playerPair1, playerPair2)) shouldEqual List(
          PotWinnings(
            potSize = 60,
            participants = Set(playerHighCard1.player.playerId, playerHighCard2.player.playerId),
            winners = Set(playerHighCard1.player.playerId, playerHighCard2.player.playerId),
          ),
          PotWinnings(
            potSize = 80,
            participants = Set(playerHighCard1.player.playerId, playerHighCard2.player.playerId, playerPair1.player.playerId, playerPair2.player.playerId),
            winners = Set(playerPair1.player.playerId, playerPair2.player.playerId),
          ),
        )
      }

      "smallest all-in wins, next-smallest all-in is second, third-smallest all-in is third, fourth smallest all-in gets balance" in {
        val playerHighCard = testPlayerHand(40, Ace of Spades, Four of Diamonds, "1")
        val playerPair = testPlayerHand(30, Jack of Hearts, Two of Diamonds, "2")
        val playerTrips = testPlayerHand(20, King of Spades, King of Diamonds, "3")
        val playerStraight = testPlayerHand(10, Ten of Diamonds, Three of Spades, "4")

        winnings(List(playerHighCard, playerPair, playerTrips, playerStraight)) shouldEqual List(
          PotWinnings(
            potSize = 10,
            participants = Set(playerHighCard.player.playerId),
            winners = Set(playerHighCard.player.playerId),
          ),
          PotWinnings(
            potSize = 20,
            participants = Set(playerHighCard.player.playerId, playerPair.player.playerId),
            winners = Set(playerPair.player.playerId),
          ),
          PotWinnings(
            potSize = 30,
            participants = Set(playerHighCard.player.playerId, playerPair.player.playerId, playerTrips.player.playerId),
            winners = Set(playerTrips.player.playerId),
          ),
          PotWinnings(
            potSize = 40,
            participants = Set(playerHighCard.player.playerId, playerPair.player.playerId, playerTrips.player.playerId, playerStraight.player.playerId),
            winners = Set(playerStraight.player.playerId),
          ),
        )
      }
    }

    "property tests" - {
      "amount paid out always equals amount paid in" - {
        "2 players" in {
          forAll { (rawP1Pot: Int, rawP2Pot: Int, seed: Long) =>
            val (p1Pot, p2Pot) = (abs(rawP1Pot), abs(rawP2Pot))
            val deck = Play.deckOrder(seed)
            val c1 :: c2 :: c3 :: c4 :: c5 :: c6 :: c7 :: c8 :: c9 :: c10 :: c11 :: c12 :: _ = deck
            val player1 = testPlayerHand(p1Pot, c9, c10, "1")
            val player2 = testPlayerHand(p2Pot, c11, c12, "2")

            val results = winnings(List(player1, player2))
            results.map(_.potSize).sum shouldEqual (p1Pot + p2Pot)
          }
        }

        "3 players" in {
          forAll { (rawP1Pot: Int, rawP2Pot: Int, rawP3Pot: Int, seed: Long) =>
            val (p1Pot, p2Pot, p3Pot) = (abs(rawP1Pot), abs(rawP2Pot), abs(rawP3Pot))
            val deck = Play.deckOrder(seed)
            val c1 :: c2 :: c3 :: c4 :: c5 :: c6 :: c7 :: c8 :: c9 :: c10 :: c11 :: c12 :: c13 :: c14 :: _ = deck
            val player1 = testPlayerHand(p1Pot, c9, c10, "1")
            val player2 = testPlayerHand(p2Pot, c11, c12, "2")
            val player3 = testPlayerHand(p3Pot, c13, c14, "3")

            val results = winnings(List(player1, player2, player3))
            results.map(_.potSize).sum shouldEqual (p1Pot + p2Pot + p3Pot)
          }
        }

        "4 players" in {
          forAll { (rawP1Pot: Int, rawP2Pot: Int, rawP3Pot: Int, rawP4Pot: Int, seed: Long) =>
            val (p1Pot, p2Pot, p3Pot, p4Pot) = (abs(rawP1Pot), abs(rawP2Pot), abs(rawP3Pot), abs(rawP4Pot))
            val deck = Play.deckOrder(seed)
            val c1 :: c2 :: c3 :: c4 :: c5 :: c6 :: c7 :: c8 :: c9 :: c10 :: c11 :: c12 :: c13 :: c14 :: c15 :: c16 :: _ = deck
            val player1 = testPlayerHand(p1Pot, c9, c10, "1")
            val player2 = testPlayerHand(p2Pot, c11, c12, "2")
            val player3 = testPlayerHand(p3Pot, c13, c14, "3")
            val player4 = testPlayerHand(p4Pot, c15, c16, "4")

            val results = winnings(List(player1, player2, player3, player4))
            results.map(_.potSize).sum shouldEqual (p1Pot + p2Pot + p3Pot + p4Pot)
          }
        }
      }
    }
  }

  "playerWinnings" - {
    val p1Id = PlayerId("p1-id")
    val p2Id = PlayerId("p2-id")
    val p3Id = PlayerId("p3-id")
    val p4Id = PlayerId("p4-id")
    val playerOrder = List(p1Id, p2Id, p3Id, p4Id)
    val p1Hand = HighCard(Ace of Hearts, King of Spades, Queen of Diamonds, Jack of Clubs, Ten of Hearts)
    val p2Hand = HighCard(Ace of Hearts, King of Spades, Queen of Diamonds, Jack of Clubs, Two of Hearts)
    val p3Hand = HighCard(Ace of Hearts, King of Spades, Queen of Diamonds, Jack of Clubs, Three of Hearts)
    val p4Hand = HighCard(Ace of Hearts, King of Spades, Queen of Diamonds, Jack of Clubs, Four of Hearts)
    val playerHands = List(p1Id -> p1Hand, p2Id -> p2Hand, p3Id -> p3Hand, p4Id -> p4Hand)

    "uses correct hand for each player" in {
      playerWinnings(
        List(PotWinnings(100, Set(p1Id, p2Id, p3Id, p4Id), Set(p1Id))),
        0, playerOrder, playerHands
      ).map(pw => pw.playerId -> pw.winnings) shouldEqual List(
        p1Id -> 100,
        p2Id -> 0,
        p3Id -> 0,
        p4Id -> 0,
      )
    }

    "pays out single winner from single pot" in {
      playerWinnings(
        List(PotWinnings(100, Set(p1Id, p2Id, p3Id, p4Id), Set(p1Id))),
        0, playerOrder, playerHands
      ).map(pw => pw.playerId -> pw.winnings) shouldEqual List(
        p1Id -> 100,
        p2Id -> 0,
        p3Id -> 0,
        p4Id -> 0,
      )
    }

    "pays out single winner from multiple pots" in {
      playerWinnings(
        List(
          PotWinnings(100, Set(p1Id, p2Id, p3Id, p4Id), Set(p1Id)),
          PotWinnings(50, Set(p1Id, p2Id, p3Id), Set(p1Id)),
        ),
        0, playerOrder, playerHands
      ).map(pw => pw.playerId -> pw.winnings) shouldEqual List(
        p1Id -> 150,
        p2Id -> 0,
        p3Id -> 0,
        p4Id -> 0,
      )
    }

    "pays out multiple single winners from multiple pots" in {
      playerWinnings(
        List(
          PotWinnings(100, Set(p1Id, p2Id, p3Id, p4Id), Set(p1Id)),
          PotWinnings(50, Set(p1Id, p2Id, p3Id), Set(p2Id)),
        ),
        0, playerOrder, playerHands
      ).map(pw => pw.playerId -> pw.winnings) shouldEqual List(
        p1Id -> 100,
        p2Id -> 50,
        p3Id -> 0,
        p4Id -> 0,
      )
    }

    "pays out a single split pot with 2 winners" in {
      playerWinnings(
        List(
          PotWinnings(100, Set(p1Id, p2Id, p3Id, p4Id), Set(p1Id, p2Id)),
        ),
        0, playerOrder, playerHands
      ).map(pw => pw.playerId -> pw.winnings) shouldEqual List(
        p1Id -> 50,
        p2Id -> 50,
        p3Id -> 0,
        p4Id -> 0,
      )
    }

    "pays out a single split pot with 3 winners" in {
      playerWinnings(
        List(
          PotWinnings(99, Set(p1Id, p2Id, p3Id, p4Id), Set(p1Id, p2Id, p3Id)),
        ),
        0, playerOrder, playerHands
      ).map(pw => pw.playerId -> pw.winnings) shouldEqual List(
        p1Id -> 33,
        p2Id -> 33,
        p3Id -> 33,
        p4Id -> 0,
      )
    }

    "pays out a single split pot where all players tie" in {
      playerWinnings(
        List(
          PotWinnings(100, Set(p1Id, p2Id, p3Id, p4Id), Set(p1Id, p2Id, p3Id, p4Id)),
        ),
        0, playerOrder, playerHands
      ).map(pw => pw.playerId -> pw.winnings) shouldEqual List(
        p1Id -> 25,
        p2Id -> 25,
        p3Id -> 25,
        p4Id -> 25,
      )
    }

    "split pots with remainder" - {
      "pays left of dealer in position 0" in {
        playerWinnings(
          List(
            PotWinnings(51, Set(p1Id, p2Id, p3Id, p4Id), Set(p1Id, p2Id)),
          ),
          0, playerOrder, playerHands
        ).map(pw => pw.playerId -> pw.winnings) shouldEqual List(
          p2Id -> 26,
          p1Id -> 25,
          p3Id -> 0,
          p4Id -> 0,
        )
      }

      "pays left of dealer in position 1 (wraps to first player)" in {
        playerWinnings(
          List(
            PotWinnings(51, Set(p1Id, p2Id, p3Id, p4Id), Set(p1Id, p2Id)),
          ),
          1, playerOrder, playerHands
        ).map(pw => pw.playerId -> pw.winnings) shouldEqual List(
          p1Id -> 26,
          p2Id -> 25,
          p3Id -> 0,
          p4Id -> 0,
        )
      }

      "pays next 2 players when remainder is 2 for a three-way split pot and button is 0" in {
        playerWinnings(
          List(
            PotWinnings(62, Set(p1Id, p2Id, p3Id, p4Id), Set(p1Id, p2Id, p4Id)),
          ),
          0, playerOrder, playerHands
        ).map(pw => pw.playerId -> pw.winnings) shouldEqual List(
          p2Id -> 21,
          p4Id -> 21,
          p1Id -> 20,
          p3Id -> 0,
        )
      }

      "pays next 2 players when remainder is 2 for a three-way split pot and button is 1" in {
        playerWinnings(
          List(
            PotWinnings(62, Set(p1Id, p2Id, p3Id, p4Id), Set(p1Id, p2Id, p4Id)),
          ),
          1, playerOrder, playerHands
        ).map(pw => pw.playerId -> pw.winnings) shouldEqual List(
          p1Id -> 21,
          p4Id -> 21,
          p2Id -> 20,
          p3Id -> 0,
        )
      }

      "pays next 2 players when remainder is 2 for a three-way split pot and button is 2" in {
        playerWinnings(
          List(
            PotWinnings(62, Set(p1Id, p2Id, p3Id, p4Id), Set(p1Id, p2Id, p4Id)),
          ),
          2, playerOrder, playerHands
        ).map(pw => pw.playerId -> pw.winnings) shouldEqual List(
          p1Id -> 21,
          p2Id -> 21,
          p4Id -> 20,
          p3Id -> 0,
        )
      }
    }
  }

  "specific hand assessments" - {
    "highCard" - {
      "returns a hand containing the first five of the provided cards" in {
        forAll(nothingConnectsCardsGen()) { cards =>
          val highCardHand = highCard(cards)
          val handCards = List(
            highCardHand.highCard, highCardHand.kicker1, highCardHand.kicker2,
            highCardHand.kicker3, highCardHand.kicker4
          )
          // nothingConnectsCardsGen provides an ordered list of cards
          // so we can just compare the front 5
          handCards shouldEqual cards.take(5)
        }
      }
    }

    "pair" - {
      "if there is a pair" - {
        "it is detected" in {
          forAll(pairCardsGen()) { cards =>
            pair(cards, findDuplicateRanks(cards)) should not be empty
          }
        }

        "pair cards have the same rank" in {
          forAll(pairCardsGen()) { cards =>
            val hand = pair(cards, findDuplicateRanks(cards)).value
            hand.pair1.rank shouldEqual hand.pair2.rank
          }
        }

        "kickers are higher than the 2 discarded cards, with kicker1 highest" in {
          forAll(pairCardsGen()) { cards =>
            val hand = pair(cards, findDuplicateRanks(cards)).value
            // 2 lowest cards - exclude the pair, and drop the highest 3 that remain
            cards.filterNot(_.rank == hand.pair1.rank).drop(3) match {
              case low :: lowest :: Nil =>
                rankOrd(true)(hand.kicker1.rank) should be >= rankOrd(true)(hand.kicker2.rank)
                rankOrd(true)(hand.kicker2.rank) should be >= rankOrd(true)(hand.kicker3.rank)
                rankOrd(true)(hand.kicker3.rank) should be >= rankOrd(true)(low.rank)
                rankOrd(true)(hand.kicker3.rank) should be >= rankOrd(true)(lowest.rank)
              case _ =>
                fail("it should be possible to get the two lowest cards?!")
            }
          }
        }
      }

      "returns None for a hand with with no pair" in {
        forAll(nothingConnectsCardsGen()) { cards =>
          pair(cards, findDuplicateRanks(cards)) shouldEqual None
        }
      }

      // I don't feel strongly about whether or not this is a property that should be true,
      // let alone one that really needs to be tested.
      "if there are two pairs, returns None (this is a 2-pair hand not a pair hand)" in {
        // this assumes the 2-pair function works properly!
        forAll(twoPairCardsGen()) { cards =>
          pair(cards, findDuplicateRanks(cards)) shouldEqual None
        }
      }
    }

    "twoPair" - {
      "if a 2-pair exists" - {
        "it should be detected" in {
          forAll(twoPairCardsGen()) { cards =>
            twoPair(cards, findDuplicateRanks(cards)) should not be empty
          }
        }

        "'up' pair cards are the same rank" in {
          forAll(twoPairCardsGen()) { cards =>
            val hand = twoPair(cards, findDuplicateRanks(cards)).value
            hand.up1.rank shouldEqual hand.up2.rank
          }
        }

        "'down' pair cards are the same rank" in {
          forAll(twoPairCardsGen()) { cards =>
            val hand = twoPair(cards, findDuplicateRanks(cards)).value
            hand.down1.rank shouldEqual hand.down2.rank
          }
        }

        "'up' pair cards are a higher rank than 'down' pair cards" in {
          forAll(twoPairCardsGen()) { cards =>
            val hand = twoPair(cards, findDuplicateRanks(cards)).value
            rankOrd(true)(hand.up1.rank) should be > rankOrd(true)(hand.down1.rank)
          }
        }

        "kicker is the highest card that remains, excluding pairs" in {
          forAll(twoPairCardsGen()) { cards =>
            val hand = twoPair(cards, findDuplicateRanks(cards)).value
            val highestOther = cards.filterNot(card => Set(hand.up1.rank, hand.down1.rank).contains(card.rank)).head
            hand.kicker shouldEqual highestOther
          }
        }
      }

      "returns None if only a single pair exists" in {
        forAll(pairCardsGen()) { cards =>
          twoPair(cards, findDuplicateRanks(cards)) shouldEqual None
        }
      }

      "returns None if there is no two-pair" in {
        forAll(
          Gen.oneOf(
            nothingConnectsCardsGen(),
            // or irrelevant hand that cannot contain two pairs
            pairCardsGen(), threeOfAKindCardsGen(),
          )
        ) { cards =>
          twoPair(cards, findDuplicateRanks(cards)) shouldEqual None
        }
      }
    }

    "threeOfAKind" - {
      "if a trip exists" - {
        "it should be detected" in {
          forAll(threeOfAKindCardsGen()) { cards =>
            threeOfAKind(cards, findDuplicateRanks(cards)) should not be empty
          }
        }

        "trip cards share the same rank" in {
          forAll(threeOfAKindCardsGen()) { cards =>
            val hand = threeOfAKind(cards, findDuplicateRanks(cards)).value
            hand.trip1.rank should (equal (hand.trip2.rank) and equal (hand.trip3.rank))
          }
        }

        "kickers are highest than the two discarded cards, with kicker 1 highest" in {
          forAll(threeOfAKindCardsGen()) { cards =>
            val hand = threeOfAKind(cards, findDuplicateRanks(cards)).value
            // 2 lowest cards - exclude the pair, and drop the highest 3 that remain
            cards.filterNot(_.rank == hand.trip1.rank).drop(2) match {
              case low :: lowest :: Nil =>
                rankOrd(true)(hand.kicker1.rank) should be >= rankOrd(true)(hand.kicker2.rank)
                rankOrd(true)(hand.kicker2.rank) should be >= rankOrd(true)(low.rank)
                rankOrd(true)(hand.kicker2.rank) should be >= rankOrd(true)(lowest.rank)
              case _ =>
                fail("it should be possible to get the two lowest cards?!")
            }
          }
        }
      }

      "returns None if there is no trip present" in {
        forAll(
          Gen.oneOf(
            nothingConnectsCardsGen(),
            // or irrelevant hand that cannot contain three of a kind
            pairCardsGen(), twoPairCardsGen(),
          )
        ) { cards =>
          threeOfAKind(cards, findDuplicateRanks(cards)) shouldEqual None
        }
      }
    }

    "straight" - {
      "if a straight exists" - {
        "it should be detected" in {
          forAll(straightCardsGen()) { cards =>
            straight(cards) should not be empty
          }
        }

        "card ranks are adjacent (i.e. this is a straight)" in {
          forAll(straightCardsGen()) { cards =>
            val hand = straight(cards).value
            val highRank = rankOrd(acesHigh = true)(hand.high.rank)
            val next1Rank = rankOrd(acesHigh = true)(hand.next1.rank)
            val next2Rank = rankOrd(acesHigh = true)(hand.next2.rank)
            val next3Rank = rankOrd(acesHigh = true)(hand.next3.rank)
            val lowRank = rankOrd(acesHigh = false)(hand.low.rank)

            highRank shouldEqual (next1Rank + 1)
            next1Rank shouldEqual (next2Rank + 1)
            next2Rank shouldEqual (next3Rank + 1)
            next3Rank shouldEqual (lowRank + 1)
          }
        }

        "in a long straight, uses the highest card as the top (unless it is an ace-low straight)" in {
          forAll(longStraightGenerator) { cards =>
            val hand = straight(cards).value
            val top = cards match {
              // Ace-low straight matches the non-ace highest
              case Card(Ace, _) :: next :: Card(Six, _) :: Card(Five, _) :: Card(Four, _) :: Card(Three, _) :: Card(Two, _) :: Nil =>
                next
              // typically we match the highest card
              case top :: _ =>
                top
              case _ =>
                fail("No cards generated?!")
            }
            hand.high shouldEqual top
          }
        }

        "correctly identifies an Ace-low straight" in {
          forAll(aceLowStraightGenerator()) { cards =>
            val hand = straight(cards).value
            hand.low.rank shouldEqual Ace
          }
        }
      }

      "edge cases that used to fail" - {
        "straight with a pair" in {
          val cards = List(
            Ace of Diamonds, Queen of Diamonds,
            Five of Hearts, Four of Hearts, Three of Clubs, Two of Hearts,
            Two of Diamonds,
          )
          straight(cards) should not be empty
        }
      }

      "returns None if no straight exists" in {
        forAll(
          Gen.oneOf(
            nothingConnectsCardsGen(),
            // or irrelevant hand that cannot contain a straight
            pairCardsGen(), twoPairCardsGen(), threeOfAKindCardsGen(),
            fullHouseCardsGen(), fourOfAKindCardsGen(),
          )
        ) { cards =>
          straight(cards) shouldEqual None
        }
      }
    }

    "flush" - {
      "if a flush exists" - {
        "it should be detected" in {
          forAll(flushCardsGen()) { cards =>
            flush(cards, findDuplicateSuits(cards)) should not be empty
          }
        }

        "all cards are the same suit" in {
          forAll(flushCardsGen()) { cards =>
            val hand = flush(cards, findDuplicateSuits(cards)).value
            hand.high.suit should (
              equal (hand.next1.suit) and
                equal (hand.next2.suit) and
                equal (hand.next3.suit) and
                equal (hand.low.suit))
          }
        }

        "cards are arranged by rank" in {
          forAll(flushCardsGen()) { cards =>
            val hand = flush(cards, findDuplicateSuits(cards)).value

            val highRank = rankOrd(acesHigh = true)(hand.high.rank)
            val next1Rank = rankOrd(acesHigh = true)(hand.next1.rank)
            val next2Rank = rankOrd(acesHigh = true)(hand.next2.rank)
            val next3Rank = rankOrd(acesHigh = true)(hand.next3.rank)
            val lowRank = rankOrd(acesHigh = true)(hand.low.rank)

            highRank should be > next1Rank
            next1Rank should be > next2Rank
            next2Rank should be > next3Rank
            next3Rank should be > lowRank
          }
        }
      }

      "returns None if no flush exists" in {
        forAll(
          Gen.oneOf(
            nothingConnectsCardsGen(),
            // or irrelevant hand that cannot contain a flush
            pairCardsGen(), twoPairCardsGen(), threeOfAKindCardsGen(),
            straightCardsGen(), fullHouseCardsGen(), fourOfAKindCardsGen(),
          )
        ) { cards =>
          flush(cards, findDuplicateSuits(cards)) shouldEqual None
        }
      }
    }

    "full house" - {
      "if a full house exists" - {
        "it should be detected" in {
          forAll(fullHouseCardsGen()) { cards =>
            fullHouse(cards, findDuplicateRanks(cards)) should not be empty
          }
        }

        "trips have the same rank" in {
          forAll(fullHouseCardsGen()) { cards =>
            val hand = fullHouse(cards, findDuplicateRanks(cards)).value
            hand.trip1.rank should (equal (hand.trip2.rank) and equal (hand.trip3.rank))
          }
        }

        "pair have the same rank" in {
          forAll(fullHouseCardsGen()) { cards =>
            val hand = fullHouse(cards, findDuplicateRanks(cards)).value
            hand.pair1.rank shouldEqual hand.pair2.rank
          }
        }

        "if two trips are present, use the lower as the pair" in {
          forAll(twoTripsGen) { cards =>
            val hand = fullHouse(cards, findDuplicateRanks(cards)).value
            val tripRank = rankOrd(acesHigh = true)(hand.trip1.rank)
            val pairRank = rankOrd(acesHigh = true)(hand.pair1.rank)
            tripRank should be > pairRank
          }
        }
      }

      "returns None if no full house exists" in {
        forAll(
          Gen.oneOf(nothingConnectsCardsGen(),
            // or irrelevant hand that cannot contain a flush
            pairCardsGen(), twoPairCardsGen(), threeOfAKindCardsGen(),
            straightCardsGen(), flushCardsGen(), straightFlushCardsGen(),
          )
        ) { cards =>
          fullHouse(cards, findDuplicateRanks(cards)) shouldEqual None
        }
      }
    }

    "fourOfAKind" - {
      "if a four-of-a-kind exists" - {
        "it should bne detected" in {
          forAll(fourOfAKindCardsGen()) { cards =>
            fourOfAKind(cards, findDuplicateRanks(cards)) should not be empty
          }
        }

        "quads all have same rank" in {
          forAll(fourOfAKindCardsGen()) { cards =>
            val hand = fourOfAKind(cards, findDuplicateRanks(cards)).value
            hand.quad1.rank should (
              equal (hand.quad2.rank) and
              equal (hand.quad3.rank) and
              equal (hand.quad4.rank)
            )
          }
        }

        "kicker is higher than the discarded two cards" in {
          forAll(fourOfAKindCardsGen()) { cards =>
            val hand = fourOfAKind(cards, findDuplicateRanks(cards)).value
            // 2 lowest cards - exclude the quad, and drop the highest card that remains
            cards.filterNot(_.rank == hand.quad1.rank).drop(1) match {
              case low :: lowest :: Nil =>
                rankOrd(true)(hand.kicker.rank) should be >= rankOrd(true)(low.rank)
                rankOrd(true)(hand.kicker.rank) should be >= rankOrd(true)(lowest.rank)
              case _ =>
                fail("it should be possible to get the two lowest cards?!")
            }
          }
        }
      }

      "returns None if there is no four-of-a-kind present" in {
        forAll(
          Gen.oneOf(
            nothingConnectsCardsGen(),
            // or irrelevant hand with no quads
            pairCardsGen(), twoPairCardsGen(), threeOfAKindCardsGen(),
            straightCardsGen(), flushCardsGen(), fullHouseCardsGen(),
            straightFlushCardsGen(),
          )
        ) { cards =>
          fourOfAKind(cards, findDuplicateRanks(cards)) shouldEqual None
        }
      }
    }

    "straightFlush" - {
      "if a straight flush is present" - {
        "it should be detected" in {
          forAll(straightFlushCardsGen()) { cards =>
            straightFlush(findDuplicateSuits(cards)) should not be empty
          }
        }

        "all cards have the same suit" in {
          forAll(straightFlushCardsGen()) { cards =>
            val hand = straightFlush(findDuplicateSuits(cards)).value
            hand.high.suit should (
              equal(hand.next1.suit) and
                equal(hand.next2.suit) and
                equal(hand.next3.suit) and
                equal(hand.low.suit))
          }
        }

        "in a long straight, uses the highest card as the top" in {
          forAll(longStraightFlushGenerator) { cards =>
            val hand = straightFlush(findDuplicateSuits(cards)).value
            val top = cards match {
              // Ace-low straight matches the non-ace highest
              case Card(Ace, _) :: next :: Card(Six, _) :: Card(Five, _) :: Card(Four, _) :: Card(Three, _) :: Card(Two, _) :: Nil =>
                next
              // typically we match the highest card
              case top :: _ =>
                top
              case _ =>
                fail("No cards generated?!")
            }
            hand.high shouldEqual top
          }
        }

        "works for an Ace-low straight flush" in {
          forAll(aceLowStraightFlushGenerator()) { cards =>
            val hand = straightFlush(findDuplicateSuits(cards)).value
            hand.low.rank shouldEqual Ace
          }
        }

        "ranks are all adjacent" in {
          forAll(straightFlushCardsGen()) { cards =>
            val hand = straightFlush(findDuplicateSuits(cards)).value
            val highRank = rankOrd(acesHigh = true)(hand.high.rank)
            val next1Rank = rankOrd(acesHigh = true)(hand.next1.rank)
            val next2Rank = rankOrd(acesHigh = true)(hand.next2.rank)
            val next3Rank = rankOrd(acesHigh = true)(hand.next3.rank)
            val lowRank = rankOrd(acesHigh = false)(hand.low.rank)

            highRank shouldEqual (next1Rank + 1)
            next1Rank shouldEqual (next2Rank + 1)
            next2Rank shouldEqual (next3Rank + 1)
            next3Rank shouldEqual (lowRank + 1)
          }
        }
      }

      "useful cases that used to fail" - {
        "ace-low straight flush" in {
          val cards = List(
            Ace of Hearts, Jack of Spades, Nine of Diamonds,
            Five of Hearts, Four of Hearts, Three of Hearts, Two of Hearts,
          )
          straightFlush(findDuplicateSuits(cards)) should not be empty
        }
      }

      "returns None if no straight flush is present" in {
        forAll(
          Gen.oneOf(
            nothingConnectsCardsGen(),
            // or irrelevant hand with no straight flush
            pairCardsGen(), twoPairCardsGen(), threeOfAKindCardsGen(),
            straightCardsGen(), flushCardsGen(), fullHouseCardsGen(),
            fourOfAKindCardsGen(),
          )
        ) { cards =>
          straightFlush(findDuplicateSuits(cards)) shouldEqual None
        }
      }
    }
  }

  "findDuplicateRanks" - {
    "if no duplicates are found, returns the all the cards individually" in {
      forAll(nothingConnectsCardsGen()) {
        case cards @ c1 :: c2 :: c3 :: c4 :: c5 :: c6 :: c7 :: Nil =>
          findDuplicateRanks(cards).toMap shouldEqual Map(
            c1.rank -> List(c1),
            c2.rank -> List(c2),
            c3.rank -> List(c3),
            c4.rank -> List(c4),
            c5.rank -> List(c5),
            c6.rank -> List(c6),
            c7.rank -> List(c7),
          )
        case _ =>
          fail("invalid card generation")
      }
    }

    "if a duplicate exists, they are grouped" in {
      forAll(nothingConnectsCardsGen()) {
        case cards @ c1 :: c2 :: _ =>
          val cr1 = c1.rank of Spades
          val cr2 = c1.rank of Clubs
          val cardsByRank = findDuplicateRanks(List(cr1, cr2, c2))
          cardsByRank.toMap.get(c1.rank).value.toSet shouldEqual Set(cr1, cr2)
        case _ =>
          fail("invalid card generation")
      }
    }
  }

  "findDuplicateSuits" - {
    "if no duplicates are found, returns all the cards by themselves" in {
      val cardsBySuit = findDuplicateSuits(List(Two of Hearts, Three of Spades, Four of Clubs, Five of Diamonds))
      cardsBySuit shouldEqual Map(
        Hearts -> List(Two of Hearts),
        Spades -> List(Three of Spades),
        Clubs -> List(Four of Clubs),
        Diamonds -> List(Five of Diamonds),
      )
    }

    "if a duplicate exists, they are grouped" in {
      val cardsBySuit = findDuplicateSuits(List(Two of Hearts, Three of Hearts, Four of Clubs, Five of Hearts))
      cardsBySuit.get(Hearts).value shouldEqual List(Two of Hearts, Three of Hearts, Five of Hearts)
    }
  }

  "handOrd" - {
    // these tests are very verbose and tedious
    // this behaviour powers "winnings", which is well-tested
  }

  "cardOrd" - {
    "orders example correctly" in {
      val cards = List(
        Ten of Spades,
        King of Hearts,
        Eight of Diamonds,
        Two of Clubs,
        Four of Spades,
      )
      cards.sortBy(cardOrd(acesHigh = true)) shouldEqual List(
        Two of Clubs,
        Four of Spades,
        Eight of Diamonds,
        Ten of Spades,
        King of Hearts,
      )
    }

    "Ace is ordered to the top if acesHigh is true" in {
      val cards = List(
        Three of Spades,
        Ace of Hearts,
        Five of Clubs,
        Eight of Diamonds,
        Four of Spades,
      )
      cards.sortBy(cardOrd(acesHigh = true)) shouldEqual List(
        Three of Spades,
        Four of Spades,
        Five of Clubs,
        Eight of Diamonds,
        Ace of Hearts,
      )
    }

    "Ace is ordered to the bottom if acesHigh is false" in {
      val cards = List(
        Eight of Spades,
        Jack of Hearts,
        Ace of Clubs,
        Nine of Hearts,
        Ten of Hearts,
      )
      cards.sortBy(cardOrd(acesHigh = false)) shouldEqual List(
        Ace of Clubs,
        Eight of Spades,
        Nine of Hearts,
        Ten of Hearts,
        Jack of Hearts,
      )
    }

    "Equal rank cards are sorted by suit" in {
      forAll { rank: Rank =>
        val cardsOfRank = List(rank of Clubs, rank of Diamonds, rank of Hearts, rank of Spades)
        val shuffled = Random.shuffle(cardsOfRank)
        shuffled.sortBy(cardOrd(true)) shouldEqual cardsOfRank
      }
    }
  }

  "rankOrd" - {
    "orders ranks with ace low" in {
      forAll { seed: Long =>
        val shuffledRanks = new Random(seed).shuffle(
          Cards.deck.map(_.rank).distinct
        )
        shuffledRanks.sortBy(rankOrd(acesHigh = false)) shouldEqual List(
          Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King
        )
      }
    }

    "orders ranks with ace high" in {
      forAll { seed: Long =>
        val shuffledRanks = new Random(seed).shuffle(
          Cards.deck.map(_.rank).distinct
        )
        shuffledRanks.sortBy(rankOrd(acesHigh = true)) shouldEqual List(
          Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace
        )
      }
    }
  }

  "suitOrd" - {
    "orders suits sensibly" in {
      forAll { seed: Long =>
        val shuffledSuits = new Random(seed).shuffle(
          List(Diamonds, Clubs, Hearts, Spades)
        )
        shuffledSuits.sortBy(suitOrd) shouldEqual List(
          Clubs, Diamonds, Hearts, Spades
        )
      }
    }
  }
}
