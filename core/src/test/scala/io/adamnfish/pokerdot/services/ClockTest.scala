package io.adamnfish.pokerdot.services

import io.adamnfish.pokerdot.TestHelpers
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers


class ClockTest extends AnyFreeSpec with Matchers with TestHelpers {
  "production clock implementation" - {
    "now increases as time increases" in {
      val oldNow = Clock.now.value()
      Thread.sleep(10)
      val newNow = Clock.now.value()
      oldNow should be < newNow
    }
  }
}
