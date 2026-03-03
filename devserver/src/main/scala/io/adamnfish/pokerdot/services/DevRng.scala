package io.adamnfish.pokerdot.services

import cats.Applicative

import scala.util.Random


/**
 * Rng that requires a fixed start value
 */
class DevRng[F[_] : Applicative](initialSeed: Long) extends Rng[F] {
  override def randomState: F[Long] = {
    Applicative[F].pure(initialSeed)
  }

  override def nextState(state: Long): F[Long] = {
    Applicative[F].pure(new Random(state).nextLong())
  }
}
