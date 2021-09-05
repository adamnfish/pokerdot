package io.adamnfish.pokerdot.services

import java.time.ZonedDateTime


trait Clock {
  val now: () => Long
}

object Clock extends Clock {
  override val now: () => Long = () => ZonedDateTime.now().toInstant.toEpochMilli
}
