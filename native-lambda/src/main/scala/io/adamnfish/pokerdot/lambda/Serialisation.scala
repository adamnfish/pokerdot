package io.adamnfish.pokerdot.lambda

import cats.Functor.ops.toAllFunctorOps
import io.circe.Json.JString
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder, Json, JsonObject, KeyEncoder, parser}
import io.circe.syntax._
import zio.IO


object Serialisation {
  implicit val requestIdentityDecoder: Decoder[RequestIdentity] = deriveDecoder
  implicit val requestContextDecoder: Decoder[RequestContext] = deriveDecoder
  implicit val requestEventDecoder: Decoder[RequestEvent] = deriveDecoder

  implicit val lambdaResponseEncoder: Encoder[LambdaResponse] = deriveEncoder
}
