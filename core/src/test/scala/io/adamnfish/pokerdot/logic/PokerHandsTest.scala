package io.adamnfish.pokerdot.logic

import io.adamnfish.pokerdot.{PokerGenerators, TestHelpers}
import io.adamnfish.pokerdot.logic.Cards.RichRank
import io.adamnfish.pokerdot.logic.PokerHands.{allRanks, allSuits, bestHand, cardOrd, findDuplicateSuits, findDuplicates, flush, fourOfAKind, fullHouse, highCard, pair, rankOrd, straight, straightFlush, threeOfAKind, twoPair}
import io.adamnfish.pokerdot.models._
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

    "for simple cases" - {
      "two players involved, clear winner" ignore {}
      "three players involved, clear winner" ignore {}
      "some players folded" ignore {}
      "some players out the game (busted)" ignore {}
    }

    "for split pots" - {
      "two players split a pot" ignore {}
      "two players split a pot that doesn't divide by 2 (balance goes left of dealer)" ignore {}
      "three players split a pot" ignore {}
      "three players split a pot that doesn't divide by 3 (balance goes left of dealer)" ignore {}
    }

    "when a player is all-in" - {
      "all-in player loses, another player can win whole balance" ignore {}
      "winning player was all-in, second place gets the balance" ignore {}
      "side-pot gets split between 2 players, balance goes to a lesser winner" ignore {}
      "side-pot gets split between 2 players, balance is split between 2 tied lesser players" ignore {}
      "smallest all-in wins, next-smallest all-in is second, third-smallest all-in is third, fourth smallest all-in gets balance" ignore {}
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
