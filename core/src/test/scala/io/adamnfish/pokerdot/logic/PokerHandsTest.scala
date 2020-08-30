package io.adamnfish.pokerdot.logic

import io.adamnfish.pokerdot.{PokerGenerators, TestHelpers}
import io.adamnfish.pokerdot.logic.Cards.RichRank
import io.adamnfish.pokerdot.logic.PokerHands.{cardOrd, findDuplicates, highCard, pair, rankOrd, straight, twoPair}
import io.adamnfish.pokerdot.models._
import org.scalacheck.Gen
import org.scalatest.OptionValues
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.util.Random


class PokerHandsTest extends AnyFreeSpec with Matchers with ScalaCheckDrivenPropertyChecks with TestHelpers with OptionValues with PokerGenerators {

  "hand assessments" - {
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

        "returns the highest three cards as kickers, with kicker1 highest" in {
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
            threeOfAKindCardsGen,
          )
        ) { cards =>
          twoPair(cards, findDuplicates(cards)) shouldEqual None
        }
      }
    }

    "threeOfAKind" - {}

    "straight" - {
      "if a straight exists" - {
        "card one higher than top card is not present (that would mean there's a higher straight)" ignore {}
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

    "flush" - {}

    "full house" - {
      // if there are two trips, uses the lower for the pair
    }

    "fourOfAKind" - {}

    "straightFlush" - {

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
