package io.adamnfish.pokerdot.logic

import io.adamnfish.pokerdot.{PokerGenerators, TestHelpers}
import io.adamnfish.pokerdot.logic.Cards.RichRank
import io.adamnfish.pokerdot.logic.PokerHands.{bestHand, cardOrd, findDuplicateSuits, findDuplicates, flush, fourOfAKind, fullHouse, highCard, pair, rankOrd, straight, straightFlush, threeOfAKind, twoPair, winnings}
import io.adamnfish.pokerdot.models._
import io.adamnfish.pokerdot.utils.Rng
import io.adamnfish.pokerdot.utils.IntHelpers.abs
import org.scalacheck.Gen
import org.scalatest.OptionValues
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.util.Random


class PokerHandsTest extends AnyFreeSpec with Matchers with ScalaCheckDrivenPropertyChecks with TestHelpers with OptionValues with PokerGenerators {
  "bestHand" - {
    "returns 'high card' when nothing better exists" in {
      forAll(nothingConnectsCardsGen) {
        case card1 :: card2 :: card3 :: card4 :: card5 :: card6 :: card7 :: Nil =>
          val hand = bestHand(card1, card2, card3, card4, card5, card6, card7)
          hand shouldBe a [HighCard]
        case _ =>
          fail("generator did not provide 7 cards")
      }
    }

    "returns 'pair' for cards containing a pair" in {
      forAll(pairCardsGen) {
        case card1 :: card2 :: card3 :: card4 :: card5 :: card6 :: card7 :: Nil =>
          val hand = bestHand(card1, card2, card3, card4, card5, card6, card7)
          hand shouldBe a [Pair]
        case _ =>
          fail("generator did not provide 7 cards")
      }
    }

    "returns 'two pair' for cards containing two pairs" in {
      forAll(twoPairCardsGen) {
        case card1 :: card2 :: card3 :: card4 :: card5 :: card6 :: card7 :: Nil =>
          val hand = bestHand(card1, card2, card3, card4, card5, card6, card7)
          hand shouldBe a [TwoPair]
        case _ =>
          fail("generator did not provide 7 cards")
      }
    }

    "returns 'three of a kind' for cards containing a trip" in {
      forAll(threeOfAKindCardsGen) {
        case card1 :: card2 :: card3 :: card4 :: card5 :: card6 :: card7 :: Nil =>
          val hand = bestHand(card1, card2, card3, card4, card5, card6, card7)
          hand shouldBe a [ThreeOfAKind]
        case _ =>
          fail("generator did not provide 7 cards")
      }
    }

    "returns 'straight' for cards containing a straight" in {
      forAll(straightCardsGen) {
        case card1 :: card2 :: card3 :: card4 :: card5 :: card6 :: card7 :: Nil =>
          val hand = bestHand(card1, card2, card3, card4, card5, card6, card7)
          hand shouldBe a [Straight]
        case _ =>
          fail("generator did not provide 7 cards")
      }
    }

    "returns 'flush' for cards containing a flush" in {
      forAll(flushCardsGen) {
        case card1 :: card2 :: card3 :: card4 :: card5 :: card6 :: card7 :: Nil =>
          val hand = bestHand(card1, card2, card3, card4, card5, card6, card7)
          hand shouldBe a [Flush]
        case _ =>
          fail("generator did not provide 7 cards")
      }
    }

    "returns 'full house' for cards containing a full house" in {
      forAll(fullHouseCardsGen) {
        case card1 :: card2 :: card3 :: card4 :: card5 :: card6 :: card7 :: Nil =>
          val hand = bestHand(card1, card2, card3, card4, card5, card6, card7)
          hand shouldBe a [FullHouse]
        case _ =>
          fail("generator did not provide 7 cards")
      }
    }

    "returns 'four of a kind' for cards containing a quad" in {
      forAll(fourOfAKindCardsGen) {
        case card1 :: card2 :: card3 :: card4 :: card5 :: card6 :: card7 :: Nil =>
          val hand = bestHand(card1, card2, card3, card4, card5, card6, card7)
          hand shouldBe a [FourOfAKind]
        case _ =>
          fail("generator did not provide 7 cards")
      }
    }

    "returns 'straight flush' for cards containing a straight flush" in {
      forAll(straightFlushCardsGen) {
        case card1 :: card2 :: card3 :: card4 :: card5 :: card6 :: card7 :: Nil =>
          val hand = bestHand(card1, card2, card3, card4, card5, card6, card7)
          hand shouldBe a [StraightFlush]
        case _ =>
          fail("generator did not provide 7 cards")
      }
    }
  }

  "winnings" - {
    // The simple cases handle the vast majority of games.
    // The edge cases get box-of-birds-mad, and need to be handled as well.
    // There are some detailed comments in the code

    val gameId = GameId("game-id")
    val round = Round(
      Showdown, Ace of Hearts, // burnt
      King of Clubs, Queen of Diamonds, Jack of Spades, // flop
        Ten of Hearts, // burnt
      Nine of Clubs, // turn
        Eight of Diamonds, // burnt
      Seven of Hearts, // river
    )

    def testPlayer(pot: Int, card1: Card, card2: Card, id: String, folded: Boolean = false): Player = {
      Player(
        gameId, PlayerId(s"player-$id"), 0, PlayerAddress(s"player-$id-address"), PlayerKey(s"$id"), s"Player $id", 1000,
        pot = pot, 0,
        folded = folded, false,
        hole = Some(Hole(card1, card2)), false
      )
    }
    def bustedPlayer(id: String): Player = {
      Player(
        gameId, PlayerId(s"player-$id"), 0, PlayerAddress(s"player-$id-address"), PlayerKey(s"$id"), s"Player $id", 1000,
        pot = 0, 0, folded = false,
        busted = true,
        hole = None, false
      )
    }

    "for simple cases" - {
      "two players involved, clear winner" in {
        val playerPair = testPlayer(50, King of Spades, Four of Hearts, "1")
        val playerHighCard = testPlayer(50, Five of Spades, Four of Clubs, "2")

        winnings(round, List(playerPair, playerHighCard)) shouldEqual List(
          PotWinnings(
            potSize = 100,
            participants = Set(playerPair.playerId, playerHighCard.playerId),
            winners = Set(playerPair.playerId)
          )
        )
      }

      "three players involved, clear winner" in {
        val playerHighCard = testPlayer(50, Five of Spades, Four of Clubs, "1")
        val playerPair = testPlayer(50, King of Spades, Four of Hearts, "2")
        val playerTrips = testPlayer(50, Seven of Clubs, Seven of Hearts, "3")

        winnings(round, List(playerPair, playerHighCard, playerTrips)) shouldEqual List(
          PotWinnings(
            potSize = 150,
            participants = Set(playerHighCard.playerId, playerPair.playerId, playerTrips.playerId),
            winners = Set(playerTrips.playerId)
          )
        )
      }

      "a player folded" in {
        val playerHighCard = testPlayer(50, Five of Spades, Four of Clubs, "1")
        val playerPair = testPlayer(50, King of Spades, Four of Hearts, "2")
        val playerTrips = testPlayer(50, Seven of Clubs, Seven of Hearts, "3")
        val foldedPlayer = testPlayer(25, Two of Hearts, Four of Diamonds, "4", folded = true)

        winnings(round, List(playerPair, playerHighCard, playerTrips, foldedPlayer)) shouldEqual List(
          PotWinnings(
            potSize = 175,
            participants = Set(playerHighCard.playerId, playerPair.playerId, playerTrips.playerId),
            winners = Set(playerTrips.playerId)
          )
        )
      }

      "multiple players folded" in {
        val playerHighCard = testPlayer(50, Five of Spades, Four of Clubs, "1")
        val playerPair = testPlayer(50, King of Spades, Four of Hearts, "2")
        val foldedPlayer = testPlayer(25, Two of Hearts, Four of Diamonds, "4", folded = true)
        val foldedPlayer2 = testPlayer(25, Two of Spades, Four of Clubs, "4", folded = true)

        winnings(round, List(playerPair, playerHighCard, foldedPlayer, foldedPlayer2)) shouldEqual List(
          PotWinnings(
            potSize = 150,
            participants = Set(playerHighCard.playerId, playerPair.playerId),
            winners = Set(playerPair.playerId)
          )
        )
      }

      "only one player remains" in {
        val playerHighCard = testPlayer(50, Five of Spades, Four of Clubs, "1")
        val foldedPlayer1 = testPlayer(50, Three of Spades, Four of Hearts, "2", folded = true)
        val foldedPlayer2 = testPlayer(25, Two of Hearts, Four of Diamonds, "4", folded = true)
        val foldedPlayer3 = testPlayer(25, Two of Spades, Four of Clubs, "4", folded = true)

        winnings(round, List(playerHighCard, foldedPlayer1, foldedPlayer2, foldedPlayer3)) shouldEqual List(
          PotWinnings(
            potSize = 150,
            participants = Set(playerHighCard.playerId),
            winners = Set(playerHighCard.playerId)
          )
        )
      }

      "if player with strongest hand folds, they do not win" in {
        val playerHighCard = testPlayer(50, Five of Spades, Four of Clubs, "1")
        val playerPair = testPlayer(50, King of Spades, Four of Hearts, "2")
        val foldedPlayerTrips = testPlayer(25, Seven of Clubs, Seven of Hearts, "3", folded = true)

        winnings(round, List(playerPair, playerHighCard, foldedPlayerTrips)) shouldEqual List(
          PotWinnings(
            potSize = 125,
            participants = Set(playerHighCard.playerId, playerPair.playerId),
            winners = Set(playerPair.playerId)
          )
        )
      }

      "unaffected by players out the game (busted)" in {
        val playerHighCard = testPlayer(50, Five of Spades, Four of Clubs, "1")
        val playerPair = testPlayer(50, King of Spades, Four of Hearts, "2")
        val playerBusted = bustedPlayer("3")

        winnings(round, List(playerPair, playerHighCard, playerBusted)) shouldEqual List(
          PotWinnings(
            potSize = 100,
            participants = Set(playerHighCard.playerId, playerPair.playerId),
            winners = Set(playerPair.playerId)
          )
        )
      }

      "heads up, player folds" in {
        val playerBlind = testPlayer(1, King of Spades, Four of Hearts, "1")
        val playerFold = testPlayer(0, Five of Spades, Four of Clubs, "2", folded = true)

        winnings(round, List(playerBlind, playerFold)) shouldEqual List(
          PotWinnings(
            potSize = 1,
            participants = Set(playerBlind.playerId),
            winners = Set(playerBlind.playerId)
          )
        )
      }
    }

    "for split pots" - {
      "two players split a pot" in {
        val playerHighCard = testPlayer(50, Five of Spades, Four of Clubs, "1")
        val playerPair1 = testPlayer(50, King of Spades, Four of Hearts, "2")
        val playerPair2 = testPlayer(50, King of Diamonds, Four of Spades, "3")

        winnings(round, List(playerHighCard, playerPair1, playerPair2)) shouldEqual List(
          PotWinnings(
            potSize = 150,
            participants = Set(playerHighCard.playerId, playerPair1.playerId, playerPair2.playerId),
            winners = Set(playerPair1.playerId, playerPair2.playerId)
          )
        )
      }

      "three players split a pot" in {
        val playerHighCard = testPlayer(50, Five of Spades, Three of Clubs, "1")
        val playerPair1 = testPlayer(50, King of Spades, Four of Hearts, "2")
        val playerPair2 = testPlayer(50, King of Diamonds, Four of Spades, "3")
        val playerPair3 = testPlayer(50, King of Hearts, Four of Clubs, "4")

        winnings(round, List(playerHighCard, playerPair1, playerPair2, playerPair3)) shouldEqual List(
          PotWinnings(
            potSize = 200,
            participants = Set(playerHighCard.playerId, playerPair1.playerId, playerPair2.playerId, playerPair3.playerId),
            winners = Set(playerPair1.playerId, playerPair2.playerId, playerPair3.playerId)
          )
        )
      }
    }

    "when a player is all-in (side-pots)" - {
      "winning player was all-in, second place gets the balance" in {
        val playerHighCard = testPlayer(50, Five of Spades, Four of Clubs, "1")
        val playerPair = testPlayer(50, King of Spades, Four of Hearts, "2")
        val playerTrips = testPlayer(10, Seven of Clubs, Seven of Hearts, "3")

        winnings(round, List(playerHighCard, playerPair, playerTrips)) shouldEqual List(
          PotWinnings(
            potSize = 80,
            participants = Set(playerHighCard.playerId, playerPair.playerId),
            winners = Set(playerPair.playerId),
          ),
          PotWinnings(
            potSize = 30,
            participants = Set(playerHighCard.playerId, playerPair.playerId, playerTrips.playerId),
            winners = Set(playerTrips.playerId),
          ),
        )
      }

      "all-in player loses, another player can win whole balance" in {
        val playerHighCard = testPlayer(10, Five of Spades, Four of Clubs, "1")
        val playerPair = testPlayer(50, King of Spades, Four of Hearts, "2")
        val playerTrips = testPlayer(50, Seven of Clubs, Seven of Hearts, "3")

        winnings(round, List(playerHighCard, playerPair, playerTrips)) shouldEqual List(
          PotWinnings(
            potSize = 80,
            participants = Set(playerPair.playerId, playerTrips.playerId),
            winners = Set(playerTrips.playerId),
          ),
          PotWinnings(
            potSize = 30,
            participants = Set(playerHighCard.playerId, playerPair.playerId, playerTrips.playerId),
            winners = Set(playerTrips.playerId),
          ),
        )
      }

      "side-pot gets split between 2 players, balance goes to a lesser winner" in {
        val playerHighCard = testPlayer(50, Five of Spades, Four of Clubs, "1")
        val playerHigherCard = testPlayer(50, Ace of Spades, Four of Clubs, "2")
        val playerPair1 = testPlayer(10, King of Spades, Four of Hearts, "3")
        val playerPair2 = testPlayer(10, King of Diamonds, Four of Spades, "4")

        winnings(round, List(playerHighCard, playerHigherCard, playerPair1, playerPair2)) shouldEqual List(
          PotWinnings(
            potSize = 80,
            participants = Set(playerHighCard.playerId, playerHigherCard.playerId),
            winners = Set(playerHigherCard.playerId),
          ),
          PotWinnings(
            potSize = 40,
            participants = Set(playerHighCard.playerId, playerHigherCard.playerId, playerPair1.playerId, playerPair2.playerId),
            winners = Set(playerPair1.playerId, playerPair2.playerId),
          ),
        )
      }

      "side-pot gets split between 2 players, balance is split between 2 tied lesser players" in {
        val playerHighCard1 = testPlayer(50, Ace of Spades, Four of Diamonds, "1")
        val playerHighCard2 = testPlayer(50, Ace of Diamonds, Four of Clubs, "2")
        val playerPair1 = testPlayer(20, King of Spades, Four of Hearts, "3")
        val playerPair2 = testPlayer(20, King of Diamonds, Four of Spades, "4")

        winnings(round, List(playerHighCard1, playerHighCard2, playerPair1, playerPair2)) shouldEqual List(
          PotWinnings(
            potSize = 60,
            participants = Set(playerHighCard1.playerId, playerHighCard2.playerId),
            winners = Set(playerHighCard1.playerId, playerHighCard2.playerId),
          ),
          PotWinnings(
            potSize = 80,
            participants = Set(playerHighCard1.playerId, playerHighCard2.playerId, playerPair1.playerId, playerPair2.playerId),
            winners = Set(playerPair1.playerId, playerPair2.playerId),
          ),
        )
      }

      "smallest all-in wins, next-smallest all-in is second, third-smallest all-in is third, fourth smallest all-in gets balance" in {
        val playerHighCard = testPlayer(40, Ace of Spades, Four of Diamonds, "1")
        val playerPair = testPlayer(30, Jack of Hearts, Two of Diamonds, "2")
        val playerTrips = testPlayer(20, King of Spades, King of Diamonds, "3")
        val playerStraight = testPlayer(10, Ten of Diamonds, Three of Spades, "4")

        winnings(round, List(playerHighCard, playerPair, playerTrips, playerStraight)) shouldEqual List(
          PotWinnings(
            potSize = 10,
            participants = Set(playerHighCard.playerId),
            winners = Set(playerHighCard.playerId),
          ),
          PotWinnings(
            potSize = 20,
            participants = Set(playerHighCard.playerId, playerPair.playerId),
            winners = Set(playerPair.playerId),
          ),
          PotWinnings(
            potSize = 30,
            participants = Set(playerHighCard.playerId, playerPair.playerId, playerTrips.playerId),
            winners = Set(playerTrips.playerId),
          ),
          PotWinnings(
            potSize = 40,
            participants = Set(playerHighCard.playerId, playerPair.playerId, playerTrips.playerId, playerStraight.playerId),
            winners = Set(playerStraight.playerId),
          ),
        )
      }
    }

    "property tests" - {
      "amount paid out always equals amount paid in" - {
        "2 players" in {
          forAll { (rawP1Pot: Int, rawP2Pot: Int, seed: Long) =>
            val (p1Pot, p2Pot) = (abs(rawP1Pot), abs(rawP2Pot))
            val deck = Rng.shuffledDeck().value(seed)
            val c1 :: c2 :: c3 :: c4 :: c5 :: c6 :: c7 :: c8 :: c9 :: c10 :: c11 :: c12 :: _ = deck
            val round = Round(Showdown, c1, c2, c3, c4, c5, c6, c7, c8)
            val player1 = testPlayer(p1Pot, c9, c10, "1")
            val player2 = testPlayer(p2Pot, c11, c12, "2")

            val results = winnings(round, List(player1, player2))
            results.map(_.potSize).sum shouldEqual (p1Pot + p2Pot)
          }
        }

        "3 players" in {
          forAll { (rawP1Pot: Int, rawP2Pot: Int, rawP3Pot: Int, seed: Long) =>
            val (p1Pot, p2Pot, p3Pot) = (abs(rawP1Pot), abs(rawP2Pot), abs(rawP3Pot))
            val deck = Rng.shuffledDeck().value(seed)
            val c1 :: c2 :: c3 :: c4 :: c5 :: c6 :: c7 :: c8 :: c9 :: c10 :: c11 :: c12 :: c13 :: c14 :: _ = deck
            val round = Round(Showdown, c1, c2, c3, c4, c5, c6, c7, c8)
            val player1 = testPlayer(p1Pot, c9, c10, "1")
            val player2 = testPlayer(p2Pot, c11, c12, "2")
            val player3 = testPlayer(p3Pot, c13, c14, "3")

            val results = winnings(round, List(player1, player2, player3))
            results.map(_.potSize).sum shouldEqual (p1Pot + p2Pot + p3Pot)
          }
        }

        "4 players" in {
          forAll { (rawP1Pot: Int, rawP2Pot: Int, rawP3Pot: Int, rawP4Pot: Int, seed: Long) =>
            val (p1Pot, p2Pot, p3Pot, p4Pot) = (abs(rawP1Pot), abs(rawP2Pot), abs(rawP3Pot), abs(rawP4Pot))
            val deck = Rng.shuffledDeck().value(seed)
            val c1 :: c2 :: c3 :: c4 :: c5 :: c6 :: c7 :: c8 :: c9 :: c10 :: c11 :: c12 :: c13 :: c14 :: c15 :: c16 :: _ = deck
            val round = Round(Showdown, c1, c2, c3, c4, c5, c6, c7, c8)
            val player1 = testPlayer(p1Pot, c9, c10, "1")
            val player2 = testPlayer(p2Pot, c11, c12, "2")
            val player3 = testPlayer(p3Pot, c13, c14, "3")
            val player4 = testPlayer(p4Pot, c15, c16, "4")

            val results = winnings(round, List(player1, player2, player3, player4))
            results.map(_.potSize).sum shouldEqual (p1Pot + p2Pot + p3Pot + p4Pot)
          }
        }
      }
    }
  }

  "specific hand assessments" - {
    "highCard" - {
      "returns a hand containing the first five of the provided cards" in {
        forAll(nothingConnectsCardsGen) { cards =>
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
        "pair cards have the same rank" in {
          forAll(pairCardsGen) { cards =>
            val hand = pair(cards, findDuplicates(cards)).value
            hand.pair1.rank shouldEqual hand.pair2.rank
          }
        }

        "kickers are higher than the 2 discarded cards, with kicker1 highest" in {
          forAll(pairCardsGen) { cards =>
            val hand = pair(cards, findDuplicates(cards)).value
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
        forAll(nothingConnectsCardsGen) { cards =>
          pair(cards, findDuplicates(cards)) shouldEqual None
        }
      }

      // I don't feel strongly about whether or not this is a property that should be true,
      // let alone one that really needs to be tested.
      "if there are two pairs, returns None (this is a 2-pair hand not a pair hand)" in {
        // this assumes the 2-pair function works properly!
        forAll(twoPairCardsGen) { cards =>
          pair(cards, findDuplicates(cards)) shouldEqual None
        }
      }
    }

    "twoPair" - {
      "if a 2-pair exists" - {
        "'up' pair cards are the same rank" in {
          forAll(twoPairCardsGen) { cards =>
            val hand = twoPair(cards, findDuplicates(cards)).value
            hand.up1.rank shouldEqual hand.up2.rank
          }
        }

        "'down' pair cards are the same rank" in {
          forAll(twoPairCardsGen) { cards =>
            val hand = twoPair(cards, findDuplicates(cards)).value
            hand.down1.rank shouldEqual hand.down2.rank
          }
        }

        "'up' pair cards are a higher rank than 'down' pair cards" in {
          forAll(twoPairCardsGen) { cards =>
            val hand = twoPair(cards, findDuplicates(cards)).value
            rankOrd(true)(hand.up1.rank) should be > rankOrd(true)(hand.down1.rank)
          }
        }

        "kicker is the highest card that remains, excluding pairs" in {
          forAll(twoPairCardsGen) { cards =>
            val hand = twoPair(cards, findDuplicates(cards)).value
            val highestOther = cards.filterNot(card => Set(hand.up1.rank, hand.down1.rank).contains(card.rank)).head
            hand.kicker shouldEqual highestOther
          }
        }
      }

      "returns None if only a single pair exists" in {
        forAll(pairCardsGen) { cards =>
          twoPair(cards, findDuplicates(cards)) shouldEqual None
        }
      }

      "returns None if there is no two-pair" in {
        forAll(
          Gen.oneOf(
            nothingConnectsCardsGen,
            // or irrelevant hand that cannot contain two pairs
            pairCardsGen, threeOfAKindCardsGen,
          )
        ) { cards =>
          twoPair(cards, findDuplicates(cards)) shouldEqual None
        }
      }
    }

    "threeOfAKind" - {
      "if a trip exists" - {
        "trip cards share the same rank" in {
          forAll(threeOfAKindCardsGen) { cards =>
            val hand = threeOfAKind(cards, findDuplicates(cards)).value
            hand.trip1.rank should (equal (hand.trip2.rank) and equal (hand.trip3.rank))
          }
        }

        "kickers are highest than the two discarded cards, with kicker 1 highest" in {
          forAll(threeOfAKindCardsGen) { cards =>
            val hand = threeOfAKind(cards, findDuplicates(cards)).value
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
            nothingConnectsCardsGen,
            // or irrelevant hand that cannot contain three of a kind
            pairCardsGen, twoPairCardsGen,
          )
        ) { cards =>
          threeOfAKind(cards, findDuplicates(cards)) shouldEqual None
        }
      }
    }

    "straight" - {
      "if a straight exists" - {
        "card ranks are adjacent (i.e. this is a straight)" in {
          forAll(straightCardsGen) { cards =>
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
          forAll(aceLowStraightGenerator) { cards =>
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
            nothingConnectsCardsGen,
            // or irrelevant hand that cannot contain a straight
            pairCardsGen, twoPairCardsGen, threeOfAKindCardsGen,
            fullHouseCardsGen, fourOfAKindCardsGen,
          )
        ) { cards =>
          straight(cards) shouldEqual None
        }
      }
    }

    "flush" - {
      "if a flush exists" - {
        "all cards are the same suit" in {
          forAll(flushCardsGen) { cards =>
            val hand = flush(cards, findDuplicateSuits(cards)).value
            hand.high.suit should (
              equal (hand.next1.suit) and
                equal (hand.next2.suit) and
                equal (hand.next3.suit) and
                equal (hand.low.suit))
          }
        }

        "cards are arranged by rank" in {
          forAll(flushCardsGen) { cards =>
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
            nothingConnectsCardsGen,
            // or irrelevant hand that cannot contain a flush
            pairCardsGen, twoPairCardsGen, threeOfAKindCardsGen,
            straightCardsGen, fullHouseCardsGen, fourOfAKindCardsGen,
          )
        ) { cards =>
          flush(cards, findDuplicateSuits(cards)) shouldEqual None
        }
      }
    }

    "full house" - {
      "if a full house exists" - {
        "trips have the same rank" in {
          forAll(fullHouseCardsGen) { cards =>
            val hand = fullHouse(cards, findDuplicates(cards)).value
            hand.trip1.rank should (equal (hand.trip2.rank) and equal (hand.trip3.rank))
          }
        }

        "pair have the same rank" in {
          forAll(fullHouseCardsGen) { cards =>
            val hand = fullHouse(cards, findDuplicates(cards)).value
            hand.pair1.rank shouldEqual hand.pair2.rank
          }
        }

        "if two trips are present, use the lower as the pair" in {
          forAll(twoTripsGen) { cards =>
            val hand = fullHouse(cards, findDuplicates(cards)).value
            val tripRank = rankOrd(acesHigh = true)(hand.trip1.rank)
            val pairRank = rankOrd(acesHigh = true)(hand.pair1.rank)
            tripRank should be > pairRank
          }
        }
      }

      "returns None if no full house exists" in {
        forAll(
          Gen.oneOf(nothingConnectsCardsGen,
            // or irrelevant hand that cannot contain a flush
            pairCardsGen, twoPairCardsGen, threeOfAKindCardsGen,
            straightCardsGen, flushCardsGen, straightFlushCardsGen
          )
        ) { cards =>
          fullHouse(cards, findDuplicates(cards)) shouldEqual None
        }
      }
    }

    "fourOfAKind" - {
      "if a four-of-a-kind exists" - {
        "quads all have same rank" in {
          forAll(fourOfAKindCardsGen) { cards =>
            val hand = fourOfAKind(cards, findDuplicates(cards)).value
            hand.quad1.rank should (
              equal (hand.quad2.rank) and
              equal (hand.quad3.rank) and
              equal (hand.quad4.rank)
            )
          }
        }

        "kicker is higher than the discarded two cards" in {
          forAll(fourOfAKindCardsGen) { cards =>
            val hand = fourOfAKind(cards, findDuplicates(cards)).value
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
            nothingConnectsCardsGen,
            // or irrelevant hand with no quads
            pairCardsGen, twoPairCardsGen, threeOfAKindCardsGen,
            straightCardsGen, flushCardsGen, fullHouseCardsGen,
            straightFlushCardsGen,
          )
        ) { cards =>
          fourOfAKind(cards, findDuplicates(cards)) shouldEqual None
        }
      }
    }

    "straightFlush" - {
      "if a straight flush is present" - {
        "all cards have the same suit" in {
          forAll(straightFlushCardsGen) { cards =>
            val hand = straightFlush(cards, findDuplicateSuits(cards)).value
            hand.high.suit should (
              equal(hand.next1.suit) and
                equal(hand.next2.suit) and
                equal(hand.next3.suit) and
                equal(hand.low.suit))
          }
        }

        "in a long straight, uses the highest card as the top" in {
          forAll(longStraightFlushGenerator) { cards =>
            val hand = straightFlush(cards, findDuplicateSuits(cards)).value
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
          forAll(aceLowStraightFlushGenerator) { cards =>
            val hand = straightFlush(cards, findDuplicateSuits(cards)).value
            hand.low.rank shouldEqual Ace
          }
        }

        "ranks are all adjacent" in {
          forAll(straightFlushCardsGen) { cards =>
            val hand = straightFlush(cards, findDuplicateSuits(cards)).value
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
          straightFlush(cards, findDuplicateSuits(cards)) should not be empty
        }
      }

      "returns None if no straight flush is present" in {
        forAll(
          Gen.oneOf(
            nothingConnectsCardsGen,
            // or irrelevant hand with no straight flush
            pairCardsGen, twoPairCardsGen, threeOfAKindCardsGen,
            straightCardsGen, flushCardsGen, fullHouseCardsGen,
            fourOfAKindCardsGen,
          )
        ) { cards =>
          straightFlush(cards, findDuplicateSuits(cards)) shouldEqual None
        }
      }
    }
  }

  "cardRankOrd" - {
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
        val cardsOfRank = List(rank of Clubs, rank of Diamonds, rank of Spades, rank of Hearts)
        val shuffled = Random.shuffle(cardsOfRank)
        shuffled.sortBy(cardOrd(true)) shouldEqual cardsOfRank
      }
    }
  }
}
