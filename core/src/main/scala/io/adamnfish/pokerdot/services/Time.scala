package io.adamnfish.pokerdot.services

import java.time.ZonedDateTime
import cats.effect.IO
import cats.effect.kernel.Clock
import cats.Functor
import cats.syntax.all.*


trait Time[F[_]] {
  val now: F[Long]
}

class RealTime[F[_] : Clock : Functor] extends Time[F] {
  override val now: F[Long] =
    Clock[F].realTimeInstant.map(_.toEpochMilli)
}
