package io.adamnfish.pokerdot

import io.adamnfish.pokerdot.models.Attempt
import io.adamnfish.pokerdot.services.{Clock, Rng}
import zio.{IO, Ref}

import java.util.concurrent.atomic.AtomicLong


object TestClock extends Clock {
  override val now: Attempt[Long] = IO.succeed(0L)
}

class ConfigurableTestClock(currentTime: Long) extends Clock {
  override val now: Attempt[Long] = IO.succeed(currentTime)
}

class RunningTestClock extends Clock {
  private val state = new AtomicLong(0L)

  override val now: Attempt[Long] =
    IO.effectTotal(state.get())

  def tick(): Attempt[Unit] =
    IO.effectTotal(state.addAndGet(10))
}

object TestRng extends Rng {
  override def randomState(): Long = 1L
  override def nextState(state: Long): Long = state + 1L
}
