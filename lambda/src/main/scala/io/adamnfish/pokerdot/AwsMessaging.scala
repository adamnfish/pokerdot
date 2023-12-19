package io.adamnfish.pokerdot
import com.amazonaws.services.lambda.runtime.LambdaLogger
import io.adamnfish.pokerdot.models._
import io.adamnfish.pokerdot.services.Messaging
import software.amazon.awssdk.core.SdkBytes
import software.amazon.awssdk.services.apigatewaymanagementapi.ApiGatewayManagementApiClient
import software.amazon.awssdk.services.apigatewaymanagementapi.model.PostToConnectionRequest
import zio.ZIO

import scala.util.control.NonFatal


class AwsMessaging(client: ApiGatewayManagementApiClient, logger: LambdaLogger) extends Messaging {
  override def sendMessage(playerAddress: PlayerAddress, message: Message): Attempt[Unit] = {
    send(playerAddress, Serialisation.encodeMessage(message))
  }

  override def sendError(playerAddress: PlayerAddress, message: Failures): Attempt[Unit] = {
    send(playerAddress, Serialisation.encodeFailure(message))
  }

  private def send(playerAddress: PlayerAddress, message: String): Attempt[Unit] = {
    logger.log(s"Message (${playerAddress.address}): $message")
    val request = PostToConnectionRequest.builder
      .connectionId(playerAddress.address)
      .data(SdkBytes.fromByteArray(message.getBytes("UTF-8")))
      .build()
    ZIO.attempt(client.postToConnection(request)).mapError {
      case NonFatal(e) =>
        Failures(
          s"AWS messaging failure ${e.getMessage}",
          "Unable to send message to player",
          None,
          Some(e),
          internal = true,
        )
    }.map(_ => ())
  }
}
