package io.adamnfish.pokerdot.services

import io.adamnfish.pokerdot.models._
import io.javalin.websocket.WsContext
import zio.IO

import scala.collection.mutable


class DevMessaging(logMessage: (String, String) => Unit) extends Messaging {
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

  /**
   * send failures are internal so clients are not distracted by
   * constant warnings after someone leaves the game.
   */
  private def send(recipientId: String, body: String): Attempt[Unit] = {
    for {
      wctx <- IO.fromOption(connections.get(recipientId)).mapError(_ =>
        Failures("User not connected", "connection not found", internal = true)
      )
      _ <-
        if (wctx.session.isOpen) {
          IO.unit
        } else {
          IO.fail {
            Failures("Connection has closed", "connection closed", internal = true)
          }
        }
      result <-
        IO.effect {
          wctx.send(body)
          ()
        }.mapError { err =>
          Failures("Error sending websocket message with wctx", "could not send message", exception = Some(err), internal = true)
        }
      _ <- IO.effect(logMessage(recipientId, body)).mapError { err =>
        Failures("Error logging websocket message", "could not log message", exception = Some(err), internal = true)
      }
    } yield result
  }
}
