package io.adamnfish.pokerdot.services

import io.adamnfish.pokerdot.models.Attempt
import zio.IO

import java.time.ZonedDateTime


trait Clock {
  // the number of milliseconds since the unix epoch
  val now: Attempt[Long]
}

object Clock extends Clock {
  override val now: Attempt[Long] = IO.effectTotal(ZonedDateTime.now().toInstant.toEpochMilli)
}
