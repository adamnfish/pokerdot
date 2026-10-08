package io.adamnfish.pokerdot.logic

import io.adamnfish.pokerdot.{TestTime, TestHelpers}
import io.adamnfish.pokerdot.logic.Games.{addPlayer, newGame, newPlayer}
import io.adamnfish.pokerdot.models.{ConnectionDb, NoActionSummary, PlayerAddress, PlayerId, SelfSummary}
import org.scalatest.OptionValues
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers


class ResponsesTest extends AnyFreeSpec with Matchers with OptionValues with TestHelpers {
  "welcome" - {
    val rawGame = newGame("game name", false, 0L, 0)
    val hostAddress = PlayerAddress("host-address")
    val host = newPlayer(rawGame.gameId, "host", true, 0L)
    val game = addPlayer(rawGame, host)
    val playerAddress = PlayerAddress("player-address")
    val player = newPlayer(game.gameId, "player", false, 0L)
    val gameWithPlayer = addPlayer(game, player)
    val addresses = Map(host.playerId -> Set(hostAddress), player.playerId -> Set(playerAddress))

    "generates a welcome message for the new player" - {
      "the welcome message is on the response" in {
        val response = Responses.welcome(gameWithPlayer, player, playerAddress, addresses)
        response.messages.keys should contain(playerAddress)
      }

      "there are no other welcome messages in the response" in {
        val response = Responses.welcome(gameWithPlayer, player, playerAddress, addresses)
        response.messages.size shouldEqual 1
      }

      "the welcome message is correctly populated" in {
        val response = Responses.welcome(gameWithPlayer, player, playerAddress, addresses)
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
      val response = Responses.welcome(gameWithPlayer, player, playerAddress, addresses)
      response.statuses.keys should not contain playerAddress
    }

    "generates a status message for the host" in {
      val response = Responses.welcome(gameWithPlayer, player, playerAddress, addresses)
      response.statuses.keys should contain(hostAddress)
    }
  }

  "playerAddresses" - {
    val p1 = PlayerId("p1")
    val p2 = PlayerId("p2")
    def connection(address: String, playerId: PlayerId) = ConnectionDb("game-id", address, playerId.pid, 0L)

    "groups each player's connections" in {
      val connections = List(connection("a1", p1), connection("a2", p1), connection("b1", p2))
      Responses.playerAddresses(connections, p1, PlayerAddress("a1")) shouldEqual Map(
        p1 -> Set(PlayerAddress("a1"), PlayerAddress("a2")),
        p2 -> Set(PlayerAddress("b1")),
      )
    }

    "includes the requester's address if it isn't connected yet" in {
      val connections = List(connection("b1", p2))
      Responses.playerAddresses(connections, p1, PlayerAddress("a1")) shouldEqual Map(
        p1 -> Set(PlayerAddress("a1")),
        p2 -> Set(PlayerAddress("b1")),
      )
    }

    "gives the requester's address to the requester, rather than another player's stale connection" in {
      val connections = List(connection("a1", p2), connection("b1", p2))
      Responses.playerAddresses(connections, p1, PlayerAddress("a1")) shouldEqual Map(
        p1 -> Set(PlayerAddress("a1")),
        p2 -> Set(PlayerAddress("b1")),
      )
    }
  }

  "gameStatuses" - {
    val rawGame = newGame("game name", false, 0L, 0)
    val hostAddress = PlayerAddress("host-address")
    val host = newPlayer(rawGame.gameId, "host", true, 0L)
    val player1Address = PlayerAddress("player-1-address")
    val player1SecondAddress = PlayerAddress("player-1-second-address")
    val player1 = newPlayer(rawGame.gameId, "player1", false, 0L)
    val player2Address = PlayerAddress("player-2-address")
    val player2 = newPlayer(rawGame.gameId, "player2", false, 0L)

    val game = addPlayer(addPlayer(addPlayer(rawGame,
      host),
      player1),
      player2
    )
    val addresses = Map(
      host.playerId -> Set(hostAddress),
      player1.playerId -> Set(player1Address, player1SecondAddress),
      player2.playerId -> Set(player2Address),
    )

    "sends a game status message to every address of every player" in {
      val responses = Responses.gameStatuses(game, NoActionSummary(), addresses)
      responses.statuses.keySet should contain only(hostAddress, player1Address, player1SecondAddress, player2Address)
    }

    "sends each player their own status" in {
      val responses = Responses.gameStatuses(game, NoActionSummary(), addresses)
      responses.statuses.get(player1SecondAddress).value.self match {
        case self: SelfSummary => self.playerId shouldEqual player1.playerId
        case other => fail(s"expected a player's self summary, got $other")
      }
    }

    "does not send any specific messages" in {
      val responses = Responses.gameStatuses(game, NoActionSummary(), addresses)
      responses.messages shouldBe empty
    }

    "skips addresses that aren't for a player" in {
      val responses = Responses.gameStatuses(game, NoActionSummary(), addresses + (PlayerId("spectator") -> Set(PlayerAddress("spectator-address"))))
      responses.statuses.keySet should not contain PlayerAddress("spectator-address")
    }
  }

  "roundWinnings" - {
    val rawGame = newGame("game name", false, 0L, 0)
    val hostAddress = PlayerAddress("host-address")
    val host = newPlayer(rawGame.gameId, "host", true, 0L)
    val player1Address = PlayerAddress("player-1-address")
    val player1SecondAddress = PlayerAddress("player-1-second-address")
    val player1 = newPlayer(rawGame.gameId, "player1", false, 0L)

    val game = addPlayer(addPlayer(rawGame, host), player1)
    val addresses = Map(
      host.playerId -> Set(hostAddress),
      player1.playerId -> Set(player1Address, player1SecondAddress),
    )

    "sends winnings to every address of every player" in {
      val responses = Responses.roundWinnings(game, Nil, Nil, addresses)
      responses.messages.keySet should contain only(hostAddress, player1Address, player1SecondAddress)
    }
  }
}
