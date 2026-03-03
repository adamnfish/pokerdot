package io.adamnfish.pokerdot.services

import java.security.SecureRandom
import cats.effect.IO
import cats.effect.kernel.Sync


trait Rng[F[_]] {
  def randomState: F[Long]

  def nextState(state: Long): F[Long]
}

// TODO: switch to cats-effect's own Random 
class RandomRng[F[_] : Sync] extends Rng[F] {
  override def randomState: F[Long] = {
    Sync[F].delay(new SecureRandom().nextLong())
  }

  // True random in PROD, for each round
  override def nextState(state: Long): F[Long] = {
    Sync[F].delay(new SecureRandom().nextLong())
  }
}
