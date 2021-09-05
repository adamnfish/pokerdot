package io.adamnfish.pokerdot.services

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers


class ClockTest extends AnyFreeSpec with Matchers {
  "production clock implementation" - {
    "now increases as time increases" in {
      val oldNow = Clock.now()
      Thread.sleep(10)
      val newNow = Clock.now()
      oldNow should be < newNow
    }
  }
}
