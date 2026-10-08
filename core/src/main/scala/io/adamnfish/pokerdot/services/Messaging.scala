package io.adamnfish.pokerdot.services

import io.adamnfish.pokerdot.models.{Failures, Message, PlayerAddress}

trait Messaging[F[_]] {
  def sendMessage(playerAddress: PlayerAddress, message: Message): F[SendResult]

  def sendError(playerAddress: PlayerAddress, message: Failures): F[SendResult]
}

sealed trait SendResult
case object Sent extends SendResult
/**
 * The connection has closed, so it will never receive messages again.
 */
case object Gone extends SendResult
