package io.adamnfish.pokerdot.integration

import cats.effect.*
import cats.effect.testing.scalatest.AsyncIOSpec
import io.adamnfish.pokerdot.TestHelpers.parseReq
import io.adamnfish.pokerdot.integration.IntegrationComponents.afterGetPlayers
import io.adamnfish.pokerdot.integration.CreateGameIntegrationTest.{createGameRequest, performCreateGame}
import io.adamnfish.pokerdot.models.*
import io.adamnfish.pokerdot.{PokerDot, TestHelpers}
import org.scalatest.OptionValues
import software.amazon.awssdk.services.dynamodb.model.ConditionalCheckFailedException
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers


class PingIntegrationTest
    extends AsyncFreeSpec
    with AsyncIOSpec
    with Matchers
    with IntegrationComponents
    with TestHelpers
    with OptionValues {
  val initialSeed = 1L
  val hostAddress = PlayerAddress("host-address")
  val newHostAddress = PlayerAddress("new-host-address")

  "when the player's address has changed" - {
    "persists the new address" in appContextRes.use { (context, db) =>
      for {
        welcome <- createGameFixture(context)
        _ <- PokerDot.ping[IO](parseReq(pingRequest(welcome)), context(newHostAddress))
        playerDbs <- db.getPlayers(welcome.gameId)
      } yield playerDbs.find(_.playerId == welcome.playerId.pid).value.playerAddress shouldEqual newHostAddress.address
    }

    "does not overwrite gameplay changes made after the ping read the player" in appContextRes.use { (context, db) =>
      for {
        welcome <- createGameFixture(context)
        // simulate another request updating the player between this ping's read and its write
        concurrentDb = afterGetPlayers(db) { playerDbs =>
          for {
            gameDb <- db.getGame(welcome.gameId)
            hostDb = playerDbs.find(_.playerId == welcome.playerId.pid).value
            _ <- db.writeGameAndPlayers(gameDb.value, List(hostDb.copy(stack = 500)))
          } yield ()
        }
        pingContext = context(newHostAddress).copy(db = concurrentDb)
        response <- PokerDot.ping[IO](parseReq(pingRequest(welcome)), pingContext)
        playerDbs <- db.getPlayers(welcome.gameId)
        hostDb = playerDbs.find(_.playerId == welcome.playerId.pid).value
      } yield {
        hostDb.playerAddress shouldEqual newHostAddress.address
        hostDb.stack shouldEqual 500
        // the response reflects the stored player, not the one read before the concurrent change
        response.messages.get(newHostAddress).value.self match {
          case self: SelfSummary => self.stack shouldEqual 500
          case other => fail(s"expected a player summary, got $other")
        }
      }
    }
  }

  "fails to update the address of a player that does not exist" in appContextRes.use { (context, db) =>
    for {
      welcome <- createGameFixture(context)
      result <- db.updatePlayerAddress(welcome.gameId, PlayerId("not-a-player"), newHostAddress).attempt
      playerDbs <- db.getPlayers(welcome.gameId)
    } yield {
      result match {
        case Left(failures: Failures) =>
          failures.exception.value shouldBe a[ConditionalCheckFailedException]
        case other =>
          fail(s"expected the update to fail, got $other")
      }
      playerDbs.map(_.playerId) should not contain "not-a-player"
    }
  }

  private def createGameFixture(contextBuilder: PlayerAddress => AppContext[IO]): IO[Welcome] = {
    performCreateGame(createGameRequest, contextBuilder(hostAddress), initialSeed).map { response =>
      response.messages.get(hostAddress).value
    }
  }

  private def pingRequest(welcome: Welcome): String = {
    s"""{"operation":"ping","gameId":"${welcome.gameId.gid}","playerId":"${welcome.playerId.pid}","playerKey":"${welcome.playerKey.key}"}"""
  }
}
