package io.adamnfish.pokerdot.services

import scala.util.Random


/**
 * Rng that requires a fixed start value
 */
class DevRng(initialSeed: Long) extends Rng {
  override def randomState(): Long = {
    initialSeed
  }

  override def nextState(state: Long): Long = {
    new Random(state).nextLong()
  }
}
