package io.adamnfish.pokerdot.integration

import io.adamnfish.pokerdot.TestHelpers
import io.adamnfish.pokerdot.models.PlayerAddress
import org.scalatest.OptionValues
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers


class AbandonRoundIntegrationTest extends AnyFreeSpec with Matchers with IntegrationComponents with TestHelpers with OptionValues {
  val hostAddress = PlayerAddress("host-address")
  val player1Address = PlayerAddress("player-1-address")
  val player2Address = PlayerAddress("player-2-address")

  "can reset the first round of a game" ignore {}

  "can reset a later round" ignore {}

  "invalid requests" - {
    "an otherwise valid request fails if the player is not an admin" ignore {}
    "fails if the game has not started" ignore {}
    "fails if the player isn't in this game" ignore {}
    "fails if the player key is not correct" ignore {}

    // TODO: should we deny abandon round requests after we are in a showdown?
  }
}
