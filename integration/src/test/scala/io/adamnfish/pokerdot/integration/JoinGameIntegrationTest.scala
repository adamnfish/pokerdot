package io.adamnfish.pokerdot.integration

import io.adamnfish.pokerdot.TestHelpers.parseReq
import io.adamnfish.pokerdot.integration.CreateGameIntegrationTest.{createGameRequest, performCreateGame}
import io.adamnfish.pokerdot.integration.JoinGameIntegrationTest.{joinGameRequest, performJoinGame}
import io.adamnfish.pokerdot.models.{AppContext, Attempt, PlayerAddress, PlayerJoinedSummary, Response, Welcome}
import io.adamnfish.pokerdot.{PokerDot, TestHelpers}
import org.scalactic.source.Position
import org.scalatest.OptionValues
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers


class JoinGameIntegrationTest extends AnyFreeSpec with Matchers with IntegrationComponents with TestHelpers with OptionValues {
  val initialSeed = 1L
  val hostAddress = PlayerAddress("host-address")
  val playerAddress = PlayerAddress("player-address")

  "for a valid request" - {
    "is successful" in withAppContext { (context, _, _) =>
      val hostWelcomeMessage = createGameFixture(context).value()
      val gameCode = hostWelcomeMessage.gameCode

      performJoinGame(joinGameRequest(gameCode), context(playerAddress)) is ASuccess
    }

    "informs the host that this player has joined" in withAppContext { (context, _, _) =>
      val hostWelcomeMessage = createGameFixture(context).value()
      val gameCode = hostWelcomeMessage.gameCode

      val response = performJoinGame(joinGameRequest(gameCode), context(playerAddress)).value()
      val hostStatusMessage = response.statuses.get(hostAddress).value
      val playerWelcomeMessage = response.messages.get(playerAddress).value

      hostStatusMessage.action shouldEqual PlayerJoinedSummary(playerWelcomeMessage.playerId)
    }

    "includes the correct players in a status message sent to the host" in withAppContext { (context, _, _) =>
      val hostWelcomeMessage = createGameFixture(context).value()
      val gameCode = hostWelcomeMessage.gameCode

      val response = performJoinGame(joinGameRequest(gameCode), context(playerAddress)).value()
      val welcomeMessage = response.messages.head._2
      val hostStatusMessage = response.statuses.get(hostAddress).value

      hostStatusMessage.game.players.length shouldEqual 2
      hostStatusMessage.game.players.map(_.playerId) shouldEqual List(welcomeMessage.playerId, hostWelcomeMessage.playerId)
    }

    "does not send a game status message to the new player" in withAppContext { (context, db, _) =>
      val hostWelcomeMessage = createGameFixture(context).value()
      val gameCode = hostWelcomeMessage.gameCode

      val response = performJoinGame(joinGameRequest(gameCode), context(playerAddress)).value()
      response.statuses.keys should not contain playerAddress
    }

    "persists the new player to the database" in withAppContext { (context, db, _) =>
      val hostWelcomeMessage = createGameFixture(context).value()
      val gameCode = hostWelcomeMessage.gameCode

      val response = performJoinGame(joinGameRequest(gameCode), context(playerAddress)).value()
      val welcomeMessage = response.messages.get(playerAddress).value
      val playerDbs = db.getPlayers(welcomeMessage.gameId).value()
      val playerDb = playerDbs.find(_.playerId == welcomeMessage.playerId.pid).value

      playerDb should have(
        "gameId" as welcomeMessage.gameId.gid,
        "playerId" as welcomeMessage.playerId.pid,
        "playerAddress" as playerAddress.address,
        "playerKey" as welcomeMessage.playerKey.key,
        "screenName" as welcomeMessage.screenName,
      )
    }

    "does not persist player to the game's database entry" in withAppContext { (context, db, _) =>
      val hostWelcomeMessage = createGameFixture(context).value()
      val gameCode = hostWelcomeMessage.gameCode

      val response = performJoinGame(joinGameRequest(gameCode), context(playerAddress)).value()
      val welcomeMessage = response.messages.get(playerAddress).value
      val gameDb = db.getGame(welcomeMessage.gameId).value().value

      gameDb.playerIds should not contain welcomeMessage.playerId.pid
    }

    "can join a second player to a game" in withAppContext { (context, _, _) =>
      val hostWelcomeMessage = createGameFixture(context).value()
      val gameCode = hostWelcomeMessage.gameCode

      performJoinGame(joinGameRequest(gameCode), context(playerAddress)).value()
      performJoinGame(joinGameRequest(gameCode, "player 2"), context(PlayerAddress("player-2-addr"))).value()
    }

    "can join a third player to a game" in withAppContext { (context, _, _) =>
      val hostWelcomeMessage = createGameFixture(context).value()
      val gameCode = hostWelcomeMessage.gameCode

      performJoinGame(joinGameRequest(gameCode), context(playerAddress)).value()
      performJoinGame(joinGameRequest(gameCode, "player 2"), context(PlayerAddress("player-2-addr"))).value()
      performJoinGame(joinGameRequest(gameCode, "player 3"), context(PlayerAddress("player-3-addr"))).value()
    }

    "can join loads of players to a game" in withAppContext { (context, _, _) =>
      val hostWelcomeMessage = createGameFixture(context).value()
      val gameCode = hostWelcomeMessage.gameCode

      performJoinGame(joinGameRequest(gameCode), context(playerAddress)).value()
      performJoinGame(joinGameRequest(gameCode, "player 2"), context(PlayerAddress("player-2-addr"))).value()
      performJoinGame(joinGameRequest(gameCode, "player 3"), context(PlayerAddress("player-3-addr"))).value()
      performJoinGame(joinGameRequest(gameCode, "player 4"), context(PlayerAddress("player-4-addr"))).value()
      performJoinGame(joinGameRequest(gameCode, "player 5"), context(PlayerAddress("player-5-addr"))).value()
      performJoinGame(joinGameRequest(gameCode, "player 6"), context(PlayerAddress("player-6-addr"))).value()
      performJoinGame(joinGameRequest(gameCode, "player 7"), context(PlayerAddress("player-7-addr"))).value()
      performJoinGame(joinGameRequest(gameCode, "player 8"), context(PlayerAddress("player-8-addr"))).value()
      performJoinGame(joinGameRequest(gameCode, "player 9"), context(PlayerAddress("player-9-addr"))).value()
      performJoinGame(joinGameRequest(gameCode, "player 10"), context(PlayerAddress("player-10-addr"))).value()
      performJoinGame(joinGameRequest(gameCode, "player 11"), context(PlayerAddress("player-11-addr"))).value()
      performJoinGame(joinGameRequest(gameCode, "player 12"), context(PlayerAddress("player-12-addr"))).value()
      performJoinGame(joinGameRequest(gameCode, "player 13"), context(PlayerAddress("player-13-addr"))).value()
      performJoinGame(joinGameRequest(gameCode, "player 14"), context(PlayerAddress("player-14-addr"))).value()
      performJoinGame(joinGameRequest(gameCode, "player 15"), context(PlayerAddress("player-15-addr"))).value()
      performJoinGame(joinGameRequest(gameCode, "player 16"), context(PlayerAddress("player-16-addr"))).value()
      performJoinGame(joinGameRequest(gameCode, "player 17"), context(PlayerAddress("player-17-addr"))).value()
      performJoinGame(joinGameRequest(gameCode, "player 18"), context(PlayerAddress("player-18-addr"))).value()
      performJoinGame(joinGameRequest(gameCode, "player 19"), context(PlayerAddress("player-19-addr"))).value()
    }
  }

  "for an invalid request" - {
    "fails if the screen name is already in use" in withAppContext { (context, _, _) =>
      val hostWelcome = createGameFixture(context).value()
      val result = performJoinGame(s"""{"gameCode": "${hostWelcome.gameCode}", "screenName": "${hostWelcome.screenName}"}""", context(playerAddress))

      result is AFailure
    }

    "fails if this is a duplicate address" in withAppContext { (context, _, _) =>
      val hostWelcome = createGameFixture(context).value()
      performJoinGame(s"""{"gameCode": "${hostWelcome.gameCode}", "screenName": "player 1"}""", context(playerAddress)).value()
      val result = performJoinGame(s"""{"gameCode": "${hostWelcome.gameCode}", "screenName": "player 2"}""", context(playerAddress))

      result is AFailure
    }

    "fails (with field context) if the game code is empty" in withAppContext { (context, _, _) =>
      createGameFixture(context).value()
      val result = performJoinGame("""{"gameCode": "", "screenName": "player name"}""", context(playerAddress))

      val failureContexts = result.failures().failures.flatMap(_.context)
      failureContexts should contain("gameCode")
    }

    "fails (with field context) if the player's screen name is empty" in withAppContext { (context, _, _) =>
      val hostWelcome = createGameFixture(context).value()
      val gameCode = hostWelcome.gameCode
      val result = performJoinGame(s"""{"gameCode": "$gameCode", "screenName": ""}""", context(playerAddress))

      val failureContexts = result.failures().failures.flatMap(_.context)
      failureContexts should contain("screenName")
    }

    "fails if the game code is wrong" in withAppContext { (context, _, _) =>
      val hostWelcome = createGameFixture(context).value()
      val incorrectGameCode =
        if (hostWelcome.gameId.gid.toLowerCase.startsWith("aaaaa"))
          s"b${hostWelcome.gameCode}"
        else
          s"a${hostWelcome.gameCode}"

      val result = performJoinGame(s"""{"gameCode": "$incorrectGameCode", "screenName": "player"}""", context(playerAddress))

      result is AFailure
    }

    "fails if the JSON is not a valid join game request" in withAppContext { (context, _, _) =>
      val result = performJoinGame(s"""{"foo": 1}""", context(playerAddress))

      result is AFailure
    }
  }

  private def createGameFixture(contextBuilder: PlayerAddress => AppContext)(implicit pos: Position): Attempt[Welcome] = {
    for {
      response <- performCreateGame(createGameRequest, contextBuilder(hostAddress), initialSeed)
    } yield {
      response.messages.get(hostAddress).value
    }
  }
}
object JoinGameIntegrationTest {
  def joinGameRequest(gameCode: String, screenName: String = "player 1"): String =
    s"""{
       |  "gameCode": "$gameCode",
       |  "screenName": "$screenName"
       |}""".stripMargin

  def performJoinGame(request: String, appContext: AppContext): Attempt[Response[Welcome]] = {
    PokerDot.joinGame(parseReq(request), appContext)
  }
}
