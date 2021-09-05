package io.adamnfish.pokerdot

import io.adamnfish.pokerdot.services.{Clock, Rng}


object TestClock extends Clock {
  override val now: () => Long = () => 0L
}

class ConfigurableTestClock(currentTime: Long) extends Clock {
  override val now: () => Long = () => currentTime
}

object TestRng extends Rng {
  override def randomState(): Long = 1L
  override def nextState(state: Long): Long = state + 1L
}
