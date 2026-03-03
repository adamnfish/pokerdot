package io.adamnfish.pokerdot.integration

import io.adamnfish.pokerdot.TestHelpers.parseReq
import io.adamnfish.pokerdot.integration.CreateGameIntegrationTest.{
  createGameRequest,
  performCreateGame
}
import io.adamnfish.pokerdot.integration.JoinGameIntegrationTest.{
  joinGameRequest,
  performJoinGame
}
import io.adamnfish.pokerdot.models.{
  AppContext,
  Failures,
  PlayerAddress,
  PlayerJoinedSummary,
  Response,
  Welcome
}
import io.adamnfish.pokerdot.{PokerDot, TestHelpers}
import cats.effect.*
import org.scalactic.source.Position
import org.scalatest.OptionValues
import org.scalatest.freespec.AsyncFreeSpec
import cats.effect.testing.scalatest.AsyncIOSpec
import org.scalatest.matchers.should.Matchers

class JoinGameIntegrationTest
    extends AsyncFreeSpec
    with AsyncIOSpec
    with Matchers
    with IntegrationComponents
    with TestHelpers
    with OptionValues {
  val initialSeed = 1L
  val hostAddress = PlayerAddress("host-address")
  val playerAddress = PlayerAddress("player-address")

  "for a valid request" - {
    "is successful" in appContextRes.use { (context, _) =>
      for {
        hostWelcomeMessage <- createGameFixture(context)
        gameCode = hostWelcomeMessage.gameCode

        _ <- performJoinGame(joinGameRequest(gameCode), context(playerAddress))
      } yield assert(true)
    }

    "informs the host that this player has joined" in appContextRes.use {
      (context, _) =>
        for {
          hostWelcomeMessage <- createGameFixture(context)
          gameCode = hostWelcomeMessage.gameCode

          response <- performJoinGame(
            joinGameRequest(gameCode),
            context(playerAddress)
          )
          hostStatusMessage = response.statuses.get(hostAddress).value
          playerWelcomeMessage = response.messages.get(playerAddress).value
        } yield hostStatusMessage.action shouldEqual PlayerJoinedSummary(
          playerWelcomeMessage.playerId
        )
    }

    "includes the correct players in a status message sent to the host" in appContextRes
      .use { (context, _) =>
        for {
          hostWelcomeMessage <- createGameFixture(context)
          gameCode = hostWelcomeMessage.gameCode

          response <- performJoinGame(
            joinGameRequest(gameCode),
            context(playerAddress)
          )
          welcomeMessage = response.messages.head._2
          hostStatusMessage = response.statuses.get(hostAddress).value

          _ = hostStatusMessage.game.players.length shouldEqual 2
          _ = hostStatusMessage.game.players.map(_.playerId) shouldEqual List(
            welcomeMessage.playerId,
            hostWelcomeMessage.playerId
          )
        } yield assert(true)
      }

    "does not send a game status message to the new player" in appContextRes
      .use { (context, db) =>
        for {
          hostWelcomeMessage <- createGameFixture(context)
          gameCode = hostWelcomeMessage.gameCode

          response <- performJoinGame(
            joinGameRequest(gameCode),
            context(playerAddress)
          )
        } yield response.statuses.keys should not contain playerAddress
      }

    "persists the new player to the database" in appContextRes.use {
      (context, db) =>
        for {
          hostWelcomeMessage <- createGameFixture(context)
          gameCode = hostWelcomeMessage.gameCode

          response <- performJoinGame(
            joinGameRequest(gameCode),
            context(playerAddress)
          )
          welcomeMessage = response.messages.get(playerAddress).value
          playerDbs <- db.getPlayers(welcomeMessage.gameId)
          playerDb = playerDbs
            .find(_.playerId == welcomeMessage.playerId.pid)
            .value
        } yield playerDb should have(
          "gameId" as welcomeMessage.gameId.gid,
          "playerId" as welcomeMessage.playerId.pid,
          "playerAddress" as playerAddress.address,
          "playerKey" as welcomeMessage.playerKey.key,
          "screenName" as welcomeMessage.screenName
        )
    }

    "does not persist player to the game's database entry" in appContextRes
      .use { (context, db) =>
        for {
          hostWelcomeMessage <- createGameFixture(context)
          gameCode = hostWelcomeMessage.gameCode

          response <- performJoinGame(
            joinGameRequest(gameCode),
            context(playerAddress)
          )
          welcomeMessage = response.messages.get(playerAddress).value
          gameDbOpt <- db.getGame(welcomeMessage.gameId)
        } yield gameDbOpt.value.playerIds should not contain welcomeMessage.playerId.pid
      }

    "can join a second player to a game" in appContextRes.use { (context, _) =>
      for {
        hostWelcomeMessage <- createGameFixture(context)
        gameCode = hostWelcomeMessage.gameCode

        _ <- performJoinGame(joinGameRequest(gameCode), context(playerAddress))
        _ <- performJoinGame(
          joinGameRequest(gameCode, "player 2"),
          context(PlayerAddress("player-2-addr"))
        )
      } yield assert(true)
    }

    "can join a third player to a game" in appContextRes.use { (context, _) =>
      for {
        hostWelcomeMessage <- createGameFixture(context)
        gameCode = hostWelcomeMessage.gameCode

        _ <- performJoinGame(joinGameRequest(gameCode), context(playerAddress))
        _ <- performJoinGame(
          joinGameRequest(gameCode, "player 2"),
          context(PlayerAddress("player-2-addr"))
        )
        _ <- performJoinGame(
          joinGameRequest(gameCode, "player 3"),
          context(PlayerAddress("player-3-addr"))
        )
      } yield assert(true)
    }

    "can join loads of players to a game" in appContextRes.use { (context, _) =>
      for {
        hostWelcomeMessage <- createGameFixture(context)
        gameCode = hostWelcomeMessage.gameCode

        _ <- performJoinGame(joinGameRequest(gameCode), context(playerAddress))
        _ <- performJoinGame(
          joinGameRequest(gameCode, "player 2"),
          context(PlayerAddress("player-2-addr"))
        )
        _ <- performJoinGame(
          joinGameRequest(gameCode, "player 3"),
          context(PlayerAddress("player-3-addr"))
        )
        _ <- performJoinGame(
          joinGameRequest(gameCode, "player 4"),
          context(PlayerAddress("player-4-addr"))
        )
        _ <- performJoinGame(
          joinGameRequest(gameCode, "player 5"),
          context(PlayerAddress("player-5-addr"))
        )
        _ <- performJoinGame(
          joinGameRequest(gameCode, "player 6"),
          context(PlayerAddress("player-6-addr"))
        )
        _ <- performJoinGame(
          joinGameRequest(gameCode, "player 7"),
          context(PlayerAddress("player-7-addr"))
        )
        _ <- performJoinGame(
          joinGameRequest(gameCode, "player 8"),
          context(PlayerAddress("player-8-addr"))
        )
        _ <- performJoinGame(
          joinGameRequest(gameCode, "player 9"),
          context(PlayerAddress("player-9-addr"))
        )
        _ <- performJoinGame(
          joinGameRequest(gameCode, "player 10"),
          context(PlayerAddress("player-10-addr"))
        )
        _ <- performJoinGame(
          joinGameRequest(gameCode, "player 11"),
          context(PlayerAddress("player-11-addr"))
        )
        _ <- performJoinGame(
          joinGameRequest(gameCode, "player 12"),
          context(PlayerAddress("player-12-addr"))
        )
        _ <- performJoinGame(
          joinGameRequest(gameCode, "player 13"),
          context(PlayerAddress("player-13-addr"))
        )
        _ <- performJoinGame(
          joinGameRequest(gameCode, "player 14"),
          context(PlayerAddress("player-14-addr"))
        )
        _ <- performJoinGame(
          joinGameRequest(gameCode, "player 15"),
          context(PlayerAddress("player-15-addr"))
        )
        _ <- performJoinGame(
          joinGameRequest(gameCode, "player 16"),
          context(PlayerAddress("player-16-addr"))
        )
        _ <- performJoinGame(
          joinGameRequest(gameCode, "player 17"),
          context(PlayerAddress("player-17-addr"))
        )
        _ <- performJoinGame(
          joinGameRequest(gameCode, "player 18"),
          context(PlayerAddress("player-18-addr"))
        )
        _ <- performJoinGame(
          joinGameRequest(gameCode, "player 19"),
          context(PlayerAddress("player-19-addr"))
        )
      } yield assert(true)
    }
  }

  "for an invalid request" - {
    "fails if the screen name is already in use" in appContextRes.use {
      (context, _) =>
        val result = for {
          hostWelcome <- createGameFixture(context)
          _ <- performJoinGame(
            s"""{"gameCode": "${hostWelcome.gameCode}", "screenName": "${hostWelcome.screenName}"}""",
            context(playerAddress)
          )
        } yield ()
        result.assertThrows[Failures]
    }

    "fails if this is a duplicate address" in appContextRes.use {
      (context, _) =>
        val result = for {
          hostWelcome <- createGameFixture(context)
          _ <- performJoinGame(
            s"""{"gameCode": "${hostWelcome.gameCode}", "screenName": "player 1"}""",
            context(playerAddress)
          )
          _ <- performJoinGame(
            s"""{"gameCode": "${hostWelcome.gameCode}", "screenName": "player 2"}""",
            context(playerAddress)
          )
        } yield ()
        result.assertThrows[Failures]
    }

    "fails (with field context) if the game code is empty" in appContextRes
      .use { (context, _) =>
        for {
          _ <- createGameFixture(context)
          result <- performJoinGame(
            """{"gameCode": "", "screenName": "player name"}""",
            context(playerAddress)
          ).attempt
          failureContexts = result.failures().failures.flatMap(_.context)
        } yield failureContexts should contain("gameCode")
      }

    "fails (with field context) if the player's screen name is empty" in appContextRes
      .use { (context, _) =>
        for {
          hostWelcome <- createGameFixture(context)
          gameCode = hostWelcome.gameCode
          result <- performJoinGame(
            s"""{"gameCode": "$gameCode", "screenName": ""}""",
            context(playerAddress)
          ).attempt

          failureContexts = result.failures().failures.flatMap(_.context)
        } yield failureContexts should contain("screenName")
      }

    "fails if the game code is wrong" in appContextRes.use { (context, _) =>
      val result = for {
        hostWelcome <- createGameFixture(context)
        incorrectGameCode =
          if (hostWelcome.gameId.gid.toLowerCase.startsWith("aaaaa"))
            s"b${hostWelcome.gameCode}"
          else
            s"a${hostWelcome.gameCode}"

        result <- performJoinGame(
          s"""{"gameCode": "$incorrectGameCode", "screenName": "player"}""",
          context(playerAddress)
        )
      } yield ()
      result.assertThrows[Failures]
    }

    "fails if the JSON is not a valid join game request" in appContextRes.use {
      (context, _) =>
        performJoinGame(
          s"""{"foo": 1}""",
          context(playerAddress)
        ).assertThrows[Failures]
    }
  }

  private def createGameFixture(
      contextBuilder: PlayerAddress => AppContext[IO]
  )(implicit pos: Position): IO[Welcome] = {
    for {
      response <- performCreateGame(
        createGameRequest,
        contextBuilder(hostAddress),
        initialSeed
      )
    } yield {
      response.messages.get(hostAddress).value
    }
  }
}
object JoinGameIntegrationTest {
  def joinGameRequest(
      gameCode: String,
      screenName: String = "player 1"
  ): String =
    s"""{
       |  "gameCode": "$gameCode",
       |  "screenName": "$screenName"
       |}""".stripMargin

  def performJoinGame(
      request: String,
      appContext: AppContext[IO]
  ): IO[Response[Welcome]] = {
    PokerDot.joinGame(parseReq(request), appContext)
  }
}
