package io.adamnfish.pokerdot.integration

import io.adamnfish.pokerdot.TestHelpers.parseReq
import io.adamnfish.pokerdot.integration.CreateGameIntegrationTest.performCreateGame
import io.adamnfish.pokerdot.models._
import io.adamnfish.pokerdot.{PokerDot, TestHelpers}
import org.scalatest.OptionValues
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers


class CreateGameIntegrationTest extends AnyFreeSpec with Matchers with IntegrationComponents with TestHelpers with OptionValues {
  val hostAddress = PlayerAddress("host-address")
  val initialSeed = 1L

  "for a valid request" - {
    val request =
      """{
        |  "screenName": "player name",
        |  "gameName": "game name"
        |}""".stripMargin

    "is successful" in {
      withAppContext { (context, _) =>
        performCreateGame(request, context(hostAddress), initialSeed) is ASuccess
      }
    }

    "sends a status message out to the host" in {
      withAppContext { (context, _) =>
        val response = performCreateGame(request, context(hostAddress), initialSeed).value()
        response.messages.size shouldEqual 1
      }
    }

    "returns a correct welcome message" in {
      withAppContext { (context, _) =>
        val response = performCreateGame(request, context(hostAddress), initialSeed).value()

        response.messages.get(hostAddress).value should have(
          "screenName" as "player name",
          "gameName" as "game name",
        )
      }
    }

    "returns a correct game summary" in {
      withAppContext { (context, _) =>
        val response = performCreateGame(request, context(hostAddress), initialSeed).value()
        val gameSummary = response.messages.get(hostAddress).value.game
        gameSummary should have(
          "gameName" as "game name",
          "started" as false,
          "inTurn" as None,
          "round" as PreFlopSummary(),
        )
      }
    }

    "persists the saved game to the database" - {
      "with key fields" in {
        withAppContext { (context, db) =>
          val response = performCreateGame(request, context(hostAddress), initialSeed).value()
          val welcomeMessage = response.messages.get(hostAddress).value
          val gameDb = db.getGame(welcomeMessage.gameId).value().value
          gameDb should have(
            "gameId" as welcomeMessage.gameId.gid,
            "gameName" as "game name",
            "phase" as PreFlop,
          )
        }
      }

      "with an appropriate expiry" in {
        withAppContext { (context, db) =>
          val appContext = context(hostAddress)
          val response = performCreateGame(request, appContext, initialSeed).value()
          val welcomeMessage = response.messages.get(hostAddress).value
          val gameDb = db.getGame(welcomeMessage.gameId).value().value

          gameDb.expiry shouldEqual appContext.dates.expires()
        }
      }
    }

    "persists the saved host to the database" - {
      "with some key fields" in {
        withAppContext { (context, db) =>
          val response = performCreateGame(request, context(hostAddress), initialSeed).value()
          val welcomeMessage = response.messages.get(hostAddress).value
          val hostDb = db.getPlayers(welcomeMessage.gameId).value().head
          hostDb should have(
            "playerKey" as welcomeMessage.playerKey.key,
            "playerId" as welcomeMessage.playerId.pid,
            "screenName" as "player name",
          )
        }
      }

      "with an appropriate expiry" in {
        withAppContext { (context, db) =>
          val appContext = context(hostAddress)
          val response = performCreateGame(request, appContext, initialSeed).value()
          val welcomeMessage = response.messages.get(hostAddress).value
          val hostDb = db.getPlayers(welcomeMessage.gameId).value().head

          hostDb.expiry shouldEqual appContext.dates.expires()
        }
      }
    }
  }
}
object CreateGameIntegrationTest {
  def performCreateGame(request: String, context: AppContext, seed: Long): Attempt[Response[Welcome]] = {
    PokerDot.createGame(parseReq(request), context, seed)
  }
}