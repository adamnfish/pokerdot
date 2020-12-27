package io.adamnfish.pokerdot.services

import java.time.ZonedDateTime


trait Dates {
  val now: () => Long
  val expires: () => Long
}

object Dates extends Dates {
  override val now: () => Long = () => ZonedDateTime.now().toInstant.toEpochMilli
  override val expires: () => Long = () => ZonedDateTime.now().plusDays(21).toInstant.toEpochMilli
}
