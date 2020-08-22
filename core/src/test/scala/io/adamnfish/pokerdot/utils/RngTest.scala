package io.adamnfish.pokerdot.utils

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import io.adamnfish.pokerdot.utils.Rng._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks


class RngTest extends AnyFreeSpec with Matchers with ScalaCheckDrivenPropertyChecks {
  "shuffledDeck" - {
    "returns the same deck for the same seed" in {
      forAll { seed: Long =>
        val expectedDeck = shuffledDeck().value(seed)
        val repeatedShuffles = List.fill(10)(seed).map(shuffledDeck().value)
        repeatedShuffles.toSet shouldEqual Set(expectedDeck)
      }
    }
  }

  "next" - {
    "returns the same next value from the same seed" in {
      forAll { seed: Long =>
        val expected = next.value(seed)
        val repeatedSeeds = List.fill(10)(seed).map(next.value)
        repeatedSeeds.toSet shouldEqual Set(expected)
      }
    }
  }
}
