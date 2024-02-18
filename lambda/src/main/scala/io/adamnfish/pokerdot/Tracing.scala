package io.adamnfish.pokerdot

import cats.Applicative
import io.adamnfish.pokerdot.models.TraceId


trait Tracing[F[_]]:
  val traceId: F[TraceId]

object Tracing:
  def apply[F[_]](implicit T: Tracing[F]): Tracing[F] = T

class AwsTracing[F[_] : Applicative](_traceId: TraceId) extends Tracing[F]:
  override val traceId: F[TraceId] = Applicative[F].pure(_traceId)
