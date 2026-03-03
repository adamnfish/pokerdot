package io.adamnfish.pokerdot

import cats.Applicative
import io.adamnfish.pokerdot.services.{Time, Rng}


class TestTime[F[_] : Applicative] extends Time[F] {
  override val now: F[Long] = Applicative[F].pure(0L)
}

class ConfigurableTestTime[F[_] : Applicative](currentTime: Long) extends Time[F] {
  override val now: F[Long] = Applicative[F].pure(currentTime)
}

class TestRng[F[_] : Applicative] extends Rng[F] {
  override def randomState: F[Long] = Applicative[F].pure(1L)
  override def nextState(state: Long): F[Long] = Applicative[F].pure(state + 1L)
}
