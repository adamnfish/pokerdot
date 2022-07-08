package io.adamnfish.pokerdot.integration

import io.adamnfish.pokerdot.TestHelpers.parseReq
import io.adamnfish.pokerdot.integration.CreateGameIntegrationTest.{createGameRequest, performCreateGame}
import io.adamnfish.pokerdot.models._
import io.adamnfish.pokerdot.{PokerDot, TestHelpers}
import org.scalactic.source.Position
import org.scalatest.OptionValues
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers


class CreateGameIntegrationTest extends AnyFreeSpec with Matchers with IntegrationComponents with TestHelpers with OptionValues {
  val hostAddress = PlayerAddress("host-address")
  val initialSeed = 1L

  "for a valid request" - {
    "is successful" in withAppContext { (context, _, _) =>
      performCreateGame(createGameRequest, context(hostAddress), initialSeed) is ASuccess
    }

    "sends a status message out to the host" in withAppContext { (context, _, _) =>
      val response = performCreateGame(createGameRequest, context(hostAddress), initialSeed).value()
      response.messages.size shouldEqual 1
    }

    "returns a correct welcome message" in withAppContext { (context, _, _) =>
      val response = performCreateGame(createGameRequest, context(hostAddress), initialSeed).value()

      response.messages.get(hostAddress).value should have(
        "screenName" as "host name",
        "gameName" as "game name",
      )
    }

    "returns a correct game summary" in withAppContext { (context, _, _) =>
      val response = performCreateGame(createGameRequest, context(hostAddress), initialSeed).value()
      val gameSummary = response.messages.get(hostAddress).value.game
      gameSummary should have(
        "gameName" as "game name",
        "started" as false,
        "inTurn" as None,
        "round" as PreFlopSummary(),
      )
    }

    "persists the saved game to the database" - {
      "with key fields" in withAppContext { (context, db, _) =>
        val response = performCreateGame(createGameRequest, context(hostAddress), initialSeed).value()
        val welcomeMessage = response.messages.get(hostAddress).value
        val gameDb = db.getGame(welcomeMessage.gameId).value().value
        gameDb should have(
          "gameId" as welcomeMessage.gameId.gid,
          "gameName" as "game name",
          "phase" as PreFlop,
        )
      }

      "with an appropriate expiry" in withAppContext { (context, db, _) =>
        val appContext = context(hostAddress)
        val response = performCreateGame(createGameRequest, appContext, initialSeed).value()
        val welcomeMessage = response.messages.get(hostAddress).value
        val gameDb = db.getGame(welcomeMessage.gameId).value().value
        val gameNow = appContext.clock.now.value()

        gameDb.expiry should be > gameNow
      }
    }

    "persists the saved host to the database" - {
      "with some key fields" in withAppContext { (context, db, _) =>
        val response = performCreateGame(createGameRequest, context(hostAddress), initialSeed).value()
        val welcomeMessage = response.messages.get(hostAddress).value
        val hostDb = db.getPlayers(welcomeMessage.gameId).value().head
        hostDb should have(
          "playerKey" as welcomeMessage.playerKey.key,
          "playerId" as welcomeMessage.playerId.pid,
          "screenName" as "host name",
        )
      }

      "with an appropriate expiry" in withAppContext { (context, db, _) =>
        val appContext = context(hostAddress)
        val response = performCreateGame(createGameRequest, appContext, initialSeed).value()
        val welcomeMessage = response.messages.get(hostAddress).value
        val hostDb = db.getPlayers(welcomeMessage.gameId).value().head
        val gameNow = appContext.clock.now.value()

        hostDb.expiry should be > gameNow
      }
    }
  }

  "for invalid submissions" - {
    "fails if the game name is empty" in withAppContext { (context, _, _) =>
      val appContext = context(hostAddress)
      val result = performCreateGame("""{"gameName": "", "screenName": "player"}""", appContext, initialSeed)
      result is AFailure
    }

    "fails if the player's screen name is empty" in withAppContext { (context, _, _) =>
      val appContext = context(hostAddress)
      val result = performCreateGame("""{"gameName": "game name", "screenName": ""}""", appContext, initialSeed)
      result is AFailure
    }

    "fails if the JSON is not a valid create game request" in withAppContext { (context, _, _) =>
      val appContext = context(hostAddress)
      val result = performCreateGame("""{}""", appContext, initialSeed)
      result is AFailure
    }
  }
}
object CreateGameIntegrationTest {
  val createGameRequest: String =
    """{
      |  "screenName": "host name",
      |  "gameName": "game name"
      |}""".stripMargin

  def performCreateGame(request: String, context: AppContext, seed: Long)(implicit pos: Position): Attempt[Response[Welcome]] = {
    PokerDot.createGame(parseReq(request), context, seed)
  }
}