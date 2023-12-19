package io.adamnfish.pokerdot.logic

import io.adamnfish.pokerdot.logic.Utils.orderFromList
import io.adamnfish.pokerdot.logic.Utils.RichList
import io.adamnfish.pokerdot.models.Failures
import org.scalatest.exceptions.TestFailedException
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import zio.{Exit, IO, Unsafe, ZIO}

import scala.util.Random


class UtilsTest extends AnyFreeSpec with Matchers with ScalaCheckDrivenPropertyChecks {
  "orderFromList" - {
    "leaves the list unchanged if the order is already correct" in {
      val list = List(1, 2, 3, 4)
      orderFromList(list, list)(identity) shouldEqual list
    }

    "orders the original list using the provided list" in {
      val original = List(1, 2, 3, 4)
      val order = List(4, 2, 3, 1)
      orderFromList(original, order)(identity) shouldEqual order
    }
  }

  "findIndex" - {
    "returned index is equal to the stdlib's index when present" in {
      forAll { seed: Long =>
        val rng = new Random(seed)
        val shuffled = rng.shuffle(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
        shuffled.findIndex(_ == 1) shouldEqual Some(shuffled.indexWhere(_ == 1))
      }
    }

    "returns None if the predicate is not satisfied" in {
      List(1, 2, 3).findIndex(_ == 4) shouldEqual None
    }
  }

  "ioTraverse" - {
    val testRuntime = zio.Runtime.default

    "returns successful accumulated result" in {
      val io = List(1, 2, 3).ioTraverse(i => ZIO.succeed(i - 1))
      Unsafe.unsafe { implicit unsafe =>
        testRuntime.unsafe.run(io) match {
          case Exit.Success(value) =>
            value shouldEqual List(0, 1, 2)
          case Exit.Failure(cause) =>
            fail(s"expected successful attempt, got $cause")
        }
      }
    }

    "returns all errors for failing `f`" in {
      val io = List(1, 2, 3).ioTraverse(i => ZIO.fail(Failures(s"fail $i", "failure")))
      Unsafe.unsafe { implicit unsafe =>
        testRuntime.unsafe.run(io) match {
          case Exit.Success(value) =>
            fail(s"expected failed attempt, got $value")
          case Exit.Failure(cause) =>
            cause.failures.head.failures.map(_.logMessage) shouldEqual List("fail 1", "fail 2", "fail 3")
        }
      }
    }
  }
}
