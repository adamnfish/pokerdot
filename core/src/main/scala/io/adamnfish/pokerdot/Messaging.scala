package io.adamnfish.pokerdot

import io.adamnfish.pokerdot.models.{Attempt, Failures, Message, PlayerAddress}


trait Messaging {
  def sendMessage(playerAddress: PlayerAddress, message: Message): Attempt[Unit]

  def sendError(playerAddress: PlayerAddress, message: Failures): Attempt[Unit]
}
