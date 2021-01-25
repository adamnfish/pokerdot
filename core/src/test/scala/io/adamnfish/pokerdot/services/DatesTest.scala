package io.adamnfish.pokerdot.services

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers


class DatesTest extends AnyFreeSpec with Matchers {
  "production dates implementation" - {
    "the now time increases as time increases" in {
      val oldNow = Dates.now()
      Thread.sleep(10)
      val newNow = Dates.now()
      oldNow should be < newNow
    }

    "the expiry is after 'now'" in {
      Dates.expires() should be > Dates.now()
    }
  }
}
