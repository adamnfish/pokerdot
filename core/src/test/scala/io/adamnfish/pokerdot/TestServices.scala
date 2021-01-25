package io.adamnfish.pokerdot

import io.adamnfish.pokerdot.services.{Dates, Rng}


object TestDates extends Dates {
  override val now: () => Long = () => 0L
  override val expires: () => Long = () => 100L
}

object TestRng extends Rng {
  override def randomState(): Long = 1L
  override def nextState(state: Long): Long = state + 1L
}
