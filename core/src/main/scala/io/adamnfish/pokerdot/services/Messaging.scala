package io.adamnfish.pokerdot.services

import io.adamnfish.pokerdot.models.{Failures, Message, PlayerAddress}

trait Messaging[F[_]] {
  def sendMessage(playerAddress: PlayerAddress, message: Message): F[Unit]

  def sendError(playerAddress: PlayerAddress, message: Failures): F[Unit]
}
