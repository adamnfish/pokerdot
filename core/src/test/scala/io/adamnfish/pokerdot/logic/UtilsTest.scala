package io.adamnfish.pokerdot.logic

import io.adamnfish.pokerdot.logic.Utils.orderFromList
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers


class UtilsTest extends AnyFreeSpec with Matchers {
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
}
