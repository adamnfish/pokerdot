package io.adamnfish.pokerdot.services

import java.security.SecureRandom


trait Rng {
  def randomState(): Long

  def nextState(state: Long): Long
}

class RandomRng extends Rng {
  override def randomState(): Long = {
    new SecureRandom().nextLong()
  }

  // True random in PROD, for each round
  override def nextState(state: Long): Long = {
    new SecureRandom().nextLong()
  }
}
