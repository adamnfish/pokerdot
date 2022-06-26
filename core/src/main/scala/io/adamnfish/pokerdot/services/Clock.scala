package io.adamnfish.pokerdot.services

import java.time.ZonedDateTime


trait Clock {
  // the number of milliseconds since the unix epoch
  val now: () => Long
}

object Clock extends Clock {
  override val now: () => Long = () => ZonedDateTime.now().toInstant.toEpochMilli
}
