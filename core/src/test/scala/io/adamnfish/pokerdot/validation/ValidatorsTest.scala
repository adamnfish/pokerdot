package io.adamnfish.pokerdot.validation

import io.adamnfish.pokerdot.models.{BreakLevel, RoundLevel}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import io.adamnfish.pokerdot.validation.Validators._
import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import java.util.UUID


class ValidatorsTest extends AnyFreeSpec with Matchers with ScalaCheckDrivenPropertyChecks {
  "nonEmpty" - {
    "returns no failures for a non-empty string" in {
      nonEmpty("non-empty", "context") shouldBe empty
    }

    "returns a failure if the validated string is empty" in {
      nonEmpty("", "context") should have length 1
    }
  }

  "nonEmptyList" - {
    "returns no failures for a non-empty list" in {
      nonEmptyList[Int](List(1, 2, 3), "context") shouldBe empty
    }

    "returns a failure if the validated list is empty" in {
      nonEmptyList(Nil, "context") should have length 1
    }
  }

  "isUUID" - {
    "returns no failures for a UUID" in {
      val uuid = UUID.randomUUID().toString
      isUUID(uuid, "context") shouldBe empty
    }

    "returns a failure if the validated string is not a UUID" in {
      isUUID("Not a UUID", "context") should have length 1
    }
  }

  "positiveInteger" - {
    "returns no failures for a positive integer" in {
      forAll(Gen.posNum[Int]) { i =>
        positiveInteger(i, "context") shouldBe empty
      }
    }

    "returns a failure if the validated number is -ve" in {
      forAll(Gen.negNum[Int]) { i =>
        positiveInteger(i, "context") should have length 1
      }
    }
  }

  "greaterThanZero" - {
    "returns no failures for strictly positive integers" in {
      forAll(Gen.posNum[Int]) { i =>
        whenever(i > 0) {
          greaterThanZero(i, "context") shouldBe empty
        }
      }
    }

    "fails for 0" in {
      greaterThanZero(0, "context") should have length 1
    }

    "returns a failure if the validated number is -ve" in {
      forAll(Gen.negNum[Int]) { i =>
        greaterThanZero(i, "context") should have length 1
      }
    }
  }

  "gameCode" - {
    "returns no failures for a UUID prefix" in {
      val uuid = UUID.randomUUID().toString.take(4)
      gameCode(uuid, "context") shouldBe empty
    }

    "allows o/O for 0 typos" in {
      gameCode("ooOO", "context") shouldBe empty
    }

    "returns no failures for a UUID" in {
      val uuid = UUID.randomUUID().toString
      gameCode(uuid, "context") shouldBe empty
    }

    "returns a failure if the validated string is too short" in {
      gameCode("aaa", "context") should have length 1
    }

    "returns a failure if the validated string is obviously not a game code" in {
      gameCode("Not a UUID", "context") should have length 1
    }
  }

  "minLength" - {
    "returns no failures for strings at least as long as the minimum" in {
      forAll(Gen.asciiPrintableStr, Gen.choose(0, 50)) { (str, n) =>
        minLength(n)(("a" * n) + str, "context") shouldBe empty
      }
    }

    "returns a failure if the validated string is shorter than the minimum" in {
      forAll(Gen.asciiPrintableStr, Gen.choose(1, 50)) { (str, n) =>
        minLength(n)(str.take(n - 1), "context") should have length 1
      }
    }
  }

  "maxLength" - {
    "returns no failures for strings at most as long as the maximum" in {
      forAll(Gen.asciiPrintableStr, Gen.choose(0, 50)) { (str, n) =>
        maxLength(n)(str.take(n), "context") shouldBe empty
      }
    }

    "returns a failure if the validated string is longer than the maximum" in {
      forAll(Gen.asciiPrintableStr, Gen.choose(1, 50)) { (str, n) =>
        maxLength(n - 1)(("a" * n) + str, "context") should have length 1
      }
    }
  }

  "sensibleLength" - {
    "returns no failures for a sensible length string" in {
      forAll(Gen.asciiPrintableStr) { str =>
        sensibleLength("a" + str.take(49), "context") shouldBe empty
      }
    }

    "fails for an empty string" in {
      sensibleLength("", "context") should have length 1
    }

    "fails for a very long string" in {
      sensibleLength("a" * 51, "context") should have length 1
    }
  }

  "timerLevel" - {
    "for RoundLevel" - {
      "returns no failures for a valid RoundLevel" in {
        timerLevel(RoundLevel(1, 1), "context") shouldBe empty
      }

      "fails if the duration is 0" in {
        timerLevel(RoundLevel(0, 1), "context") should have length 1
      }

      "fails if the duration is -ve" in {
        forAll(Gen.negNum[Int]) { n =>
          timerLevel(RoundLevel(n, 1), "context") should have length 1
        }
      }

      "fails if the blind is 0" in {
        timerLevel(RoundLevel(1, 0), "context") should have length 1
      }

      "fails if the blind is -ve" in {
        forAll(Gen.negNum[Int]) { n =>
          timerLevel(RoundLevel(1, n), "context") should have length 1
        }
      }
    }

    "for BreakLevel" - {
      "returns no failures for a valid BreakLevel" in {
        timerLevel(BreakLevel(1), "context") shouldBe empty
      }

      "fails if the duration is 0" in {
        timerLevel(BreakLevel(0), "context") should have length 1
      }

      "fails if the duration is -ve" in {
        forAll(Gen.negNum[Int]) { n =>
          timerLevel(BreakLevel(n), "context") should have length 1
        }
      }
    }
  }
}
