package io.adamnfish.pokerdot.services

import cats.effect.IO
import munit.CatsEffectSuite
import scala.concurrent.duration.DurationInt


class TimeTest extends CatsEffectSuite {
  test("now increases as time increases") {
    for
      oldNow <- RealTime[IO].now
      _ <- IO.sleep(2.seconds)
      newNow <- RealTime[IO].now
    yield assert(oldNow < newNow, s"$oldNow should be < $newNow")
  }
}
