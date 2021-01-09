package io.adamnfish.pokerdot

import io.adamnfish.pokerdot.logic.PokerHands.{findDuplicateSuits, findDuplicateRanks, flush, fourOfAKind, pair, straight, threeOfAKind}
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

    "Ace-low straights" - {
      "excludes an Ace from open-ended low straight draw" in {
        val banned = breakStraight(King, Nine, Five, Four, Three, Two)
        banned shouldEqual Set(Ace, Six)
      }

      "excludes gap 1" in {
        val banned = breakStraight(Ace, King, Nine, Five, Four, Three)
        banned shouldEqual Set(Two)
      }

      "excludes gap 2" in {
        val banned = breakStraight(Ace, King, Nine, Five, Four, Two)
        banned shouldEqual Set(Three)
      }

      "excludes gap 3" in {
        val banned = breakStraight(Ace, King, Nine, Five, Three, Two)
        banned shouldEqual Set(Four)
      }

      "excludes gap the Five at the top of a straight draw" in {
        val banned = breakStraight(Ace, King, Nine, Four, Three, Two)
        banned shouldEqual Set(Five)
      }
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

  "accidental hand checking" - {
    "high card / nothing connects" - {
      "does not contain a pair" in {
        forAll(nothingConnectsCardsGen()) { cards =>
          pair(cards, findDuplicateRanks(cards)) shouldEqual None
        }
      }
      "does not contain a trip" in {
        forAll(nothingConnectsCardsGen()) { cards =>
          threeOfAKind(cards, findDuplicateRanks(cards)) shouldEqual None
        }
      }
      "does not contain a straight" in {
        forAll(nothingConnectsCardsGen()) { cards =>
          straight(cards) shouldEqual None
        }
      }
      "does not contain a flush" in {
        forAll(nothingConnectsCardsGen()) { cards =>
          flush(cards, findDuplicateSuits(cards)) shouldEqual None
        }
      }
      "does not contain a quad" in {
        forAll(nothingConnectsCardsGen()) { cards =>
          fourOfAKind(cards, findDuplicateRanks(cards)) shouldEqual None
        }
      }
    }

    "pair" - {
      "does not contain a trip" in {
        forAll(pairCardsGen()) { cards =>
          threeOfAKind(cards, findDuplicateRanks(cards)) shouldEqual None
        }
      }
      "does not contain a straight" in {
        forAll(pairCardsGen()) { cards =>
          straight(cards) shouldEqual None
        }
      }
      "does not contain a flush" in {
        forAll(pairCardsGen()) { cards =>
          flush(cards, findDuplicateSuits(cards)) shouldEqual None
        }
      }
    }

    "two pair" - {
      "does not contain a trip" in {
        forAll(twoPairCardsGen()) { cards =>
          threeOfAKind(cards, findDuplicateRanks(cards)) shouldEqual None
        }
      }
      "does not contain a straight" in {
        forAll(twoPairCardsGen()) { cards =>
          straight(cards) shouldEqual None
        }
      }
      "does not contain a flush" in {
        forAll(twoPairCardsGen()) { cards =>
          flush(cards, findDuplicateSuits(cards)) shouldEqual None
        }
      }
    }

    "three of a kind" - {
      "does not contain a straight" in {
        forAll(threeOfAKindCardsGen()) { cards =>
          straight(cards) shouldEqual None
        }
      }
      "does not contain a flush" in {
        forAll(threeOfAKindCardsGen()) { cards =>
          flush(cards, findDuplicateSuits(cards)) shouldEqual None
        }
      }
    }

    "straight" - {
      "does not contain a flush" in {
        forAll(straightCardsGen()) { cards =>
          flush(cards, findDuplicateSuits(cards)) shouldEqual None
        }
      }
    }

    "flush" - {
      "does not contain a straight" in {
        forAll(flushCardsGen()) { cards =>
          straight(cards) shouldEqual None
        }
      }
    }

    "full house" - {
      "does not contain quads" in {
        forAll(fullHouseCardsGen()) { cards =>
          fourOfAKind(cards, findDuplicateRanks(cards)) shouldEqual None
        }
      }
    }

    // there aren't any accidental hands to watch out for with four of a kind
    // or straight flush, since they are such strong hands
  }
}
