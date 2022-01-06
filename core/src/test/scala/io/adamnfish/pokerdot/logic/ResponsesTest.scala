package io.adamnfish.pokerdot.logic

import io.adamnfish.pokerdot.{TestClock, TestHelpers}
import io.adamnfish.pokerdot.logic.Games.{addPlayer, newGame, newPlayer}
import io.adamnfish.pokerdot.models.{NoActionSummary, PlayerAddress}
import org.scalatest.OptionValues
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers


class ResponsesTest extends AnyFreeSpec with Matchers with OptionValues with TestHelpers {
  "welcome" - {
    val rawGame = newGame("game name", false, TestClock, 0)
    val hostAddress = PlayerAddress("host-address")
    val host = newPlayer(rawGame.gameId, "host", true, hostAddress, TestClock)
    val game = addPlayer(rawGame, host)

    "generates a welcome message for the new player" - {
      val playerAddress = PlayerAddress("player-address")
      val player = newPlayer(game.gameId, "player", false, playerAddress, TestClock)

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
      val player = newPlayer(game.gameId, "player", false, playerAddress, TestClock)
      val response = Responses.welcome(game, player, playerAddress)

      response.statuses.keys should not contain playerAddress
    }

    "generates a status message for the host" in {
      val playerAddress = PlayerAddress("player-address")
      val player = newPlayer(game.gameId, "player", false, playerAddress, TestClock)
      val response = Responses.welcome(game, player, playerAddress)

      response.statuses.keys should contain(hostAddress)
    }
  }

  "gameStatuses" - {
    val rawGame = newGame("game name", false, TestClock, 0)
    val hostAddress = PlayerAddress("host-address")
    val host = newPlayer(rawGame.gameId, "host", true, hostAddress, TestClock)
    val player1Address = PlayerAddress("player-1-address")
    val player1 = newPlayer(rawGame.gameId, "player1", false, player1Address, TestClock)
    val player2Address = PlayerAddress("player-2-address")
    val player2 = newPlayer(rawGame.gameId, "player2", false, player2Address, TestClock)

    val game = addPlayer(addPlayer(addPlayer(rawGame,
      host),
      player1),
      player2
    )

    "sends a game status message to all players" in {
      val responses = Responses.gameStatuses(game, NoActionSummary(), host.playerId, hostAddress)
      responses.statuses.keySet should contain only(hostAddress, player1Address, player2Address)
    }

    "does not send any specific messages" in {
      val responses = Responses.gameStatuses(game, NoActionSummary(), host.playerId, hostAddress)
      responses.messages shouldBe empty
    }

    "uses the active player's current (rather than persisted) address" in {
      val currentAddress = PlayerAddress("different-address")
      val responses = Responses.gameStatuses(game, NoActionSummary(), player1.playerId, currentAddress)
      responses.statuses.keySet should (
        contain(currentAddress) and
          not contain player1Address
        )
    }
  }

  "roundWinnings" - {
    val rawGame = newGame("game name", false, TestClock, 0)
    val hostAddress = PlayerAddress("host-address")
    val host = newPlayer(rawGame.gameId, "host", true, hostAddress, TestClock)
    val player1Address = PlayerAddress("player-1-address")
    val player1 = newPlayer(rawGame.gameId, "player1", false, player1Address, TestClock)
    val player2Address = PlayerAddress("player-2-address")
    val player2 = newPlayer(rawGame.gameId, "player2", false, player2Address, TestClock)

    val game = addPlayer(addPlayer(addPlayer(rawGame,
      host),
      player1),
      player2
    )

    "uses the active player's current (rather than persisted) address" in {
      val currentAddress = PlayerAddress("different-address")
      val responses = Responses.roundWinnings(game, Nil, Nil, player1.playerId, currentAddress)
      responses.messages.keySet should (
        contain(currentAddress) and
          not contain player1Address
        )
    }
  }
}
