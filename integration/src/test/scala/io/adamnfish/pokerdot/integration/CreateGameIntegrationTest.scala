package io.adamnfish.pokerdot.integration

import cats.effect.*
import cats.effect.testing.scalatest.AsyncIOSpec
import io.adamnfish.pokerdot.TestHelpers.parseReq
import io.adamnfish.pokerdot.integration.CreateGameIntegrationTest.{
  createGameRequest,
  performCreateGame
}
import io.adamnfish.pokerdot.models.*
import io.adamnfish.pokerdot.{PokerDot, TestHelpers}
import org.scalactic.source.Position
import org.scalatest.OptionValues
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers

class CreateGameIntegrationTest
    extends AsyncFreeSpec
    with AsyncIOSpec
    with Matchers
    with IntegrationComponents
    with TestHelpers
    with OptionValues {
  val hostAddress = PlayerAddress("host-address")
  val initialSeed = 1L

  "for a valid request" - {
    "is successful" in appContextRes.use { (context, _) =>
      performCreateGame(
        createGameRequest,
        context(hostAddress),
        initialSeed
      ).assertNoException
    }

    "sends a status message out to the host" in appContextRes.use { (context, _) =>
      for {
        response <- performCreateGame(
          createGameRequest,
          context(hostAddress),
          initialSeed
        )
      } yield response.messages.size shouldEqual 1
    }

    "returns a correct welcome message" in appContextRes.use { (context, _) =>
      for {
        response <- performCreateGame(
          createGameRequest,
          context(hostAddress),
          initialSeed
        )
      } yield response.messages.get(hostAddress).value should have(
        "screenName" as "host name",
        "gameName" as "game name"
      )
    }

    "returns a correct game summary" in appContextRes.use { (context, _) =>
      for {
        response <- performCreateGame(
          createGameRequest,
          context(hostAddress),
          initialSeed
        )
        gameSummary = response.messages.get(hostAddress).value.game
      } yield gameSummary should have(
        "gameName" as "game name",
        "started" as false,
        "inTurn" as None,
        "round" as PreFlopSummary()
      )
    }

    "persists the saved game to the database" - {
      "with key fields" in appContextRes.use { (context, db) =>
        for {
          response <- performCreateGame(
            createGameRequest,
            context(hostAddress),
            initialSeed
          )
          welcomeMessage = response.messages.get(hostAddress).value
          gameDbOpt <- db.getGame(welcomeMessage.gameId)
        } yield gameDbOpt.value should have(
          "gameId" as welcomeMessage.gameId.gid,
          "gameName" as "game name",
          "phase" as PreFlop
        )
      }

      "with an appropriate expiry" in appContextRes.use { (context, db) =>
        val appContext = context(hostAddress)
        for {
          response <- performCreateGame(
            createGameRequest,
            appContext,
            initialSeed
          )
          welcomeMessage = response.messages.get(hostAddress).value
          gameDbOpt <- db.getGame(welcomeMessage.gameId)
          now <- appContext.time.now
        } yield gameDbOpt.value.expiry should be > now
      }
    }

    "persists the saved host to the database" - {
      "with some key fields" in appContextRes.use { (context, db) =>
        for {
          response <- performCreateGame(
            createGameRequest,
            context(hostAddress),
            initialSeed
          )
          welcomeMessage = response.messages.get(hostAddress).value
          dbPlayers <- db.getPlayers(welcomeMessage.gameId)
          hostDb = dbPlayers.head
        } yield hostDb should have(
          "playerKey" as welcomeMessage.playerKey.key,
          "playerId" as welcomeMessage.playerId.pid,
          "screenName" as "host name"
        )
      }

      "with an appropriate expiry" in appContextRes.use { (context, db) =>
        val appContext = context(hostAddress)
        for {
          response <- performCreateGame(
            createGameRequest,
            appContext,
            initialSeed
          )
          welcomeMessage = response.messages.get(hostAddress).value
          dbPlayers <- db.getPlayers(welcomeMessage.gameId)
          hostDb = dbPlayers.head
          now <- appContext.time.now
        } yield hostDb.expiry should be > now
      }
    }
  }

  "for invalid submissions" - {
    "fails if the game name is empty" in appContextRes.use { (context, _) =>
      performCreateGame(
        """{"gameName": "", "screenName": "player"}""",
        context(hostAddress),
        initialSeed
      ).assertThrows[Failures]
    }

    "fails if the player's screen name is empty" in appContextRes.use {
      (context, _) =>
        performCreateGame(
          """{"gameName": "game name", "screenName": ""}""",
          context(hostAddress),
          initialSeed
        ).assertThrows[Failures]
    }

    "fails if the JSON is not a valid create game request" in appContextRes.use {
      (context, _) =>
        performCreateGame("""{}""", context(hostAddress), initialSeed)
          .assertThrows[Failures]
    }
  }
}
object CreateGameIntegrationTest {
  val createGameRequest: String =
    """{
      |  "screenName": "host name",
      |  "gameName": "game name"
      |}""".stripMargin

  def performCreateGame(request: String, context: AppContext[IO], seed: Long)(
      implicit pos: Position
  ): IO[Response[Welcome]] = {
    PokerDot.createGame[IO](parseReq(request), context, seed)
  }
}
