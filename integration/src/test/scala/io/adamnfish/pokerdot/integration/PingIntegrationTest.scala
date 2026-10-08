package io.adamnfish.pokerdot.integration

import cats.effect.*
import cats.effect.testing.scalatest.AsyncIOSpec
import io.adamnfish.pokerdot.TestHelpers.parseReq
import io.adamnfish.pokerdot.integration.CreateGameIntegrationTest.{createGameRequest, performCreateGame}
import io.adamnfish.pokerdot.models.*
import io.adamnfish.pokerdot.services.Database
import io.adamnfish.pokerdot.{PokerDot, TestHelpers}
import org.scalatest.OptionValues
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
          playerDbs
            .find(_.playerId == welcome.playerId.pid)
            .fold(IO.unit)(playerDb => db.writePlayer(playerDb.copy(stack = 500)))
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
      result.isLeft shouldEqual true
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

  /**
   * Wraps a database so that the provided effect runs after players are read.
   */
  private def afterGetPlayers(db: Database[IO])(effect: List[PlayerDb] => IO[Unit]): Database[IO] =
    new Database[IO] {
      override def getGame(gameId: GameId): IO[Option[GameDb]] = db.getGame(gameId)
      override def lookupGame(gameCode: String): IO[Option[GameDb]] = db.lookupGame(gameCode)
      override def searchGameCode(gameCode: String): IO[List[GameDb]] = db.searchGameCode(gameCode)
      override def getPlayers(gameId: GameId): IO[List[PlayerDb]] = db.getPlayers(gameId).flatTap(effect)
      override def writeGame(gameDB: GameDb): IO[Unit] = db.writeGame(gameDB)
      override def writePlayer(playerDB: PlayerDb): IO[Unit] = db.writePlayer(playerDB)
      override def updatePlayerAddress(gameId: GameId, playerId: PlayerId, playerAddress: PlayerAddress): IO[PlayerDb] =
        db.updatePlayerAddress(gameId, playerId, playerAddress)
    }
}
