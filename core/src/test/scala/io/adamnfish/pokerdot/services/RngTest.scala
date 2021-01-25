package io.adamnfish.pokerdot.services

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks


class RngTest extends AnyFreeSpec with Matchers with ScalaCheckDrivenPropertyChecks {
  "production RNG" - {
    "returns a random initial seed" in {
      val rng = new RandomRng
      rng.randomState() should not equal rng.randomState()
    }

    "returns a different 'next' seed every time" in {
      forAll { seed: Long =>
        val rng = new RandomRng
        rng.nextState(seed) should not equal rng.nextState(seed)
      }
    }
  }
}
