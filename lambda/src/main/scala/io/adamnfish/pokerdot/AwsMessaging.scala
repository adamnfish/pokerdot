package io.adamnfish.pokerdot

import cats.MonadThrow
import cats.effect.kernel.Sync
import com.typesafe.scalalogging.LazyLogging
import io.adamnfish.pokerdot.models.*
import io.adamnfish.pokerdot.services.{Gone, Messaging, SendResult, Sent}
import org.typelevel.log4cats.Logger
import software.amazon.awssdk.core.SdkBytes
import software.amazon.awssdk.services.apigatewaymanagementapi.ApiGatewayManagementApiClient
import software.amazon.awssdk.services.apigatewaymanagementapi.model.{GoneException, PostToConnectionRequest}

import scala.util.control.NonFatal

import cats.*
import cats.implicits.*


class AwsMessaging[F[_] : MonadThrow : Logger : Sync](client: ApiGatewayManagementApiClient, traceId: TraceId) extends Messaging[F] {
  override def sendMessage(playerAddress: PlayerAddress, message: Message): F[SendResult] = {
    send(playerAddress, Serialisation.encodeMessage(message))
  }

  override def sendError(playerAddress: PlayerAddress, message: Failures): F[SendResult] = {
    send(playerAddress, Serialisation.encodeFailure(message))
  }

  private def send(playerAddress: PlayerAddress, message: String): F[SendResult] = {
    for
      _ <- Logger[F].debug(s"<${traceId.tid}> Message {${playerAddress.address}}: $message")
      request = PostToConnectionRequest.builder
        .connectionId(playerAddress.address)
        .data(SdkBytes.fromByteArray(message.getBytes("UTF-8")))
        .build()
      result <- Sync[F].blocking(client.postToConnection(request)).as[SendResult](Sent).recover {
        case _: GoneException => Gone
      }.adaptError {
        case NonFatal(e) =>
          Failures(
            s"AWS messaging failure ${e.getMessage}",
            "Unable to send message to player",
            None,
            Some(e),
            internal = true,
          )
      }
    yield result
  }
}
