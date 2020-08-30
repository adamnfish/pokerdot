package io.adamnfish.pokerdot

import io.adamnfish.pokerdot.logic.PokerHands.{findDuplicateSuits, findDuplicates, flush, pair, straight, threeOfAKind}
import io.adamnfish.pokerdot.models._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks


class PokerGeneratorsTest extends AnyFreeSpec with Matchers with ScalaCheckDrivenPropertyChecks with PokerGenerators {
  "breakStraight" - {
    // TODO: property check these?
    "excludes ends of a 4-rank example" in {
      val banned = breakStraight(Nine, Eight, Seven, Six)
      banned shouldEqual Set(Ten, Five)
    }

    "excludes gap 1 in a broken 4-rank example" in {
      val banned = breakStraight(Ten, Eight, Seven, Six)
      banned shouldEqual Set(Nine)
    }

    "excludes gap 2 in a broken 4-rank example" in {
      val banned = breakStraight(Ten, Nine, Seven, Six)
      banned shouldEqual Set(Eight)
    }

    "excludes gap 3 in a broken 4-rank example" in {
      val banned = breakStraight(Ten, Nine, Eight, Six)
      banned shouldEqual Set(Seven)
    }

    "excludes card from > 4" in {
      val banned = breakStraight(King, Ten, Seven, Eight, Four, Five)
      banned shouldEqual Set(Six)
    }

    "example from (previously) failed test" in {
      val banned = breakStraight(Queen, Seven, Five, Ace, Jack, King)
      banned shouldEqual Set(Ten)
    }
  }

  "breakFlush" - {
    "works for this (previously failed) example" in {
      val banned = breakFlush(Diamonds, Clubs, Clubs, Diamonds, Diamonds, Diamonds)
      banned shouldEqual Set(Diamonds)
    }
  }

  "high card / nothing connects" - {
    "does not contain a pair" in {
      forAll(nothingConnectsCardsGen) { cards =>
        pair(cards, findDuplicates(cards)) shouldEqual None
      }
    }
    "does not contain a trip" in {
      forAll(nothingConnectsCardsGen) { cards =>
        threeOfAKind(cards, findDuplicates(cards)) shouldEqual None
      }
    }
    "does not contain a straight" in {
      forAll(nothingConnectsCardsGen) { cards =>
        straight(cards) shouldEqual None
      }
    }
    "does not contain a flush" in {
      forAll(nothingConnectsCardsGen) { cards =>
        flush(cards, findDuplicateSuits(cards)) shouldEqual None
      }
    }
  }

  "pair" - {
    "does not contain a trip" in {
      forAll(pairCardsGen) { cards =>
        threeOfAKind(cards, findDuplicates(cards)) shouldEqual None
      }
    }
    "does not contain a straight" in {
      forAll(pairCardsGen) { cards =>
        straight(cards) shouldEqual None
      }
    }
    "does not contain a flush" in {
      forAll(pairCardsGen) { cards =>
        flush(cards, findDuplicateSuits(cards)) shouldEqual None
      }
    }
  }

  "two pair" - {
    "does not contain a trip" in {
      forAll(twoPairCardsGen) { cards =>
        threeOfAKind(cards, findDuplicates(cards)) shouldEqual None
      }
    }
    "does not contain a straight" in {
      forAll(twoPairCardsGen) { cards =>
        straight(cards) shouldEqual None
      }
    }
    "does not contain a flush" in {
      forAll(twoPairCardsGen) { cards =>
        flush(cards, findDuplicateSuits(cards)) shouldEqual None
      }
    }
  }

  "three of a kind" - {
    "does not contain a straight" in {
      forAll(threeOfAKindCardsGen) { cards =>
        straight(cards) shouldEqual None
      }
    }
    "does not contain a flush" in {
      forAll(threeOfAKindCardsGen) { cards =>
        flush(cards, findDuplicateSuits(cards)) shouldEqual None
      }
    }
  }

  "straight" - {
    "does not contain a flush" in {
      forAll(straightCardsGen) { cards =>
        flush(cards, findDuplicateSuits(cards)) shouldEqual None
      }
    }
  }

  "flush" - {
    "does not contain a straight" in {
      forAll(flushCardsGen) { cards =>
        straight(cards) shouldEqual None
      }
    }
  }

//  "full house" - {
//  }
//
//  "four of a kind" in {
//    forAll(fourOfAKindCardsGen) { cards =>
//      println(s"four of a kind: $cards")
//    }
//  }
//
//  "straight flush" in {
//    forAll(straightFlushCardsGen) { cards =>
//      println(s"straight flush: $cards")
//    }
//  }
}
