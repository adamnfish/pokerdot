package io.adamnfish.pokerdot.services

import cats.MonadThrow
import io.adamnfish.pokerdot.models.*
import io.javalin.websocket.WsContext

import scala.collection.mutable
import cats.effect.{IO, Sync}
import cats.syntax.all.*


class DevMessaging[F[_] : Sync : MonadThrow](logMessage: (String, String) => F[Unit]) extends Messaging[F] {
  private val connections = new mutable.HashMap[String, WsContext]

  def connect(wctx: WsContext): String = {
    val playerAddress = wctx.sessionId
    connections.put(playerAddress, wctx)
    playerAddress
  }

  def disconnect(wctx: WsContext): Unit = {
    connections.find(_._2 == wctx).foreach { case (id, _) =>
      connections.remove(id)
    }
  }

  override def sendMessage(playerAddress: PlayerAddress, message: Message): F[Unit] = {
    send(playerAddress.address, Serialisation.encodeMessage(message))
  }

  override def sendError(playerAddress: PlayerAddress, message: Failures): F[Unit] = {
    send(playerAddress.address, Serialisation.encodeFailure(message))
  }

  /**
   * send failures are internal so clients are not distracted by
   * constant warnings after someone leaves the game.
   */
  private def send(recipientId: String, body: String): F[Unit] = {
    for {
      wctx <- MonadThrow[F].fromOption(
        connections.get(recipientId),
        Failures("User not connected", "connection not found", internal = true)
      )
      _ <-
        if (wctx.session.isOpen) {
          MonadThrow[F].unit
        } else {
          MonadThrow[F].raiseError {
            Failures("Connection has closed", "connection closed", internal = true)
          }
        }
      result <- {
        Sync[F].blocking {
          wctx.send(body)
          ()
        }.adaptError { case err =>
          Failures("Error sending websocket message with wctx", "could not send message", exception = Some(err), internal = true)
        }
      }
      _ <- Sync[F].blocking(logMessage(recipientId, body)).adaptError { err =>
        Failures("Error logging websocket message", "could not log message", exception = Some(err), internal = true)
      }
    } yield result
  }
}
