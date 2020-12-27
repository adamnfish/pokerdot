package io.adamnfish.pokerdot

import io.adamnfish.pokerdot.services.Dates


object TestDates extends Dates {
  override val now: () => Long = () => 0L
  override val expires: () => Long = () => 100L
}
