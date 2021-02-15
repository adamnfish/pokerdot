package io.adamnfish.pokerdot.logic

import io.adamnfish.pokerdot.{TestDates, TestHelpers}
import io.adamnfish.pokerdot.logic.Games.{addPlayer, newGame, newPlayer}
import io.adamnfish.pokerdot.models.PlayerAddress
import org.scalatest.OptionValues
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers


class ResponsesTest extends AnyFreeSpec with Matchers with OptionValues with TestHelpers {
  "welcome" - {
    val rawGame = newGame("game name", false, TestDates, 0)
    val hostAddress = PlayerAddress("host-address")
    val host = newPlayer(rawGame.gameId, "host", true, hostAddress, TestDates)
    val game = addPlayer(rawGame, host)

    "generates a welcome message for the new player" - {
      val playerAddress = PlayerAddress("player-address")
      val player = newPlayer(game.gameId, "player", false, playerAddress, TestDates)

      "the welcome message is on the response" in {
        val response = Responses.welcome(game, player, playerAddress)
        response.messages.keys should contain(playerAddress)
      }

      "there are no other welcome messages in the response" in {
        val response = Responses.welcome(game, player, playerAddress)
        response.messages.size shouldEqual 1
      }

      "the welcome message is correctly populated" in {
        val response = Responses.welcome(game, player, playerAddress)
        response.messages.head._2 should have(
          "playerKey" as player.playerKey.key,
          "playerId" as player.playerId.pid,
          "gameId" as player.gameId.gid,
          "gameName" as game.gameName,
          "screenName" as player.screenName,
          "spectator" as false,
        )
      }
    }


    "does not generate a status message for the new player" in {
      val playerAddress = PlayerAddress("player-address")
      val player = newPlayer(game.gameId, "player", false, playerAddress, TestDates)
      val response = Responses.welcome(game, player, playerAddress)

      response.statuses.keys should not contain playerAddress
    }

    "generates a status message for the host" in {
      val playerAddress = PlayerAddress("player-address")
      val player = newPlayer(game.gameId, "player", false, playerAddress, TestDates)
      val response = Responses.welcome(game, player, playerAddress)

      response.statuses.keys should contain(hostAddress)
    }
  }
}
