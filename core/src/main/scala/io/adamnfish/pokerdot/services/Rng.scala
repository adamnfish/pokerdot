package io.adamnfish.pokerdot.services

import java.security.SecureRandom
import scala.util.Random


trait Rng {
  def randomState(): Long

  def nextState(state: Long): Long
}

class RandomRng extends Rng {
  override def randomState(): Long = {
    new SecureRandom().nextLong()
  }

  override def nextState(state: Long): Long = {
    new Random(state).nextLong()
  }
}
