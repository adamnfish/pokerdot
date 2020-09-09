package io.adamnfish.pokerdot

import io.adamnfish.pokerdot.logic.Utils.Attempt
import io.adamnfish.pokerdot.models.{Attempt, Failure, Failures, Message, PlayerAddress, Serialisation}
import io.javalin.websocket.WsContext
import zio.IO

import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.util.control.NonFatal


class DevMessaging extends Messaging {
  private val connections = new mutable.HashMap[String, WsContext]

  def connect(wctx: WsContext): String = {
    val id = wctx.getSessionId
    connections.put(id, wctx)
    id
  }

  def disconnect(wctx: WsContext): Unit = {
    connections.find(_._2 == wctx).foreach { case (id, _) =>
      connections.remove(id)
    }
  }

  override def sendMessage(playerAddress: PlayerAddress, message: Message): Attempt[Unit] = {
    send(playerAddress.address, Serialisation.encodeMessage(message))
  }

  override def sendError(playerAddress: PlayerAddress, message: Failures): Attempt[Unit] = {
    send(playerAddress.address, Serialisation.encodeFailure(message))
  }

  private def send(recipientId: String, body: String): Attempt[Unit] = {
    for {
      wctx <- IO.fromOption(connections.get(recipientId)).mapError(_ =>
        Failures("User not connected", "Connection not found")
      )
      _ <-
        if (wctx.session.isOpen) {
          IO.unit
        } else {
          IO.fail {
            Failures("Connection has closed", "Connection closed")
          }
        }
      result <-
        IO.effect {
          wctx.send(body)
          ()
        }.mapError { err =>
          Failures("Error sending websocket message with wctx", "Could not send message", exception = Some(err))
        }
    } yield result
  }
}
