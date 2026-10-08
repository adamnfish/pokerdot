package io.adamnfish.pokerdot.integration

import cats.effect.*
import cats.effect.testing.scalatest.AsyncIOSpec
import io.adamnfish.pokerdot.TestHelpers.parseReq
import io.adamnfish.pokerdot.integration.CreateGameIntegrationTest.{createGameRequest, performCreateGame}
import io.adamnfish.pokerdot.integration.IntegrationComponents.afterGetPlayers
import io.adamnfish.pokerdot.integration.JoinGameIntegrationTest.{joinGameRequest, performJoinGame}
import io.adamnfish.pokerdot.integration.StartGameIntegrationTest.{performStartGame, startGameRequest}
import io.adamnfish.pokerdot.models.*
import io.adamnfish.pokerdot.services.{Gone, Messaging, SendResult, Sent}
import io.adamnfish.pokerdot.{PokerDot, TestHelpers}
import org.scalatest.OptionValues
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers


class ConnectionsIntegrationTest
    extends AsyncFreeSpec
    with AsyncIOSpec
    with Matchers
    with IntegrationComponents
    with TestHelpers
    with OptionValues {
  val initialSeed = 1L
  val hostAddress = PlayerAddress("host-address")
  val secondHostAddress = PlayerAddress("second-host-address")
  val playerAddress = PlayerAddress("player-address")

  "ping" - {
    "saves a connection from a new address" in appContextRes.use { (context, db) =>
      for {
        welcome <- createGameFixture(context)
        _ <- ping(welcome, context(secondHostAddress))
        connections <- db.getConnections(welcome.gameId)
      } yield connections.map(c => (c.address, c.playerId)).toSet shouldEqual Set(
        (hostAddress.address, welcome.playerId.pid),
        (secondHostAddress.address, welcome.playerId.pid),
      )
    }

    "does not duplicate an existing connection" in appContextRes.use { (context, db) =>
      for {
        welcome <- createGameFixture(context)
        _ <- ping(welcome, context(hostAddress))
        connections <- db.getConnections(welcome.gameId)
      } yield connections.length shouldEqual 1
    }

    "does not overwrite gameplay changes made after it read the player" in appContextRes.use { (context, db) =>
      for {
        welcome <- createGameFixture(context)
        concurrentDb = afterGetPlayers(db) { playerDbs =>
          val hostDb = playerDbs.find(_.playerId == welcome.playerId.pid).value
          db.writePlayer(hostDb.copy(stack = 500))
        }
        _ <- ping(welcome, context(secondHostAddress).copy(db = concurrentDb))
        playerDbs <- db.getPlayers(welcome.gameId)
        connections <- db.getConnections(welcome.gameId)
      } yield {
        playerDbs.find(_.playerId == welcome.playerId.pid).value.stack shouldEqual 500
        connections.map(_.address) should contain(secondHostAddress.address)
      }
    }
  }

  "updates go to all of a player's connections" in appContextRes.use { (context, _) =>
    for {
      welcome <- createGameFixture(context)
      _ <- ping(welcome, context(secondHostAddress))
      response <- performJoinGame(joinGameRequest(welcome.gameCode), context(playerAddress))
    } yield response.statuses.keySet shouldEqual Set(hostAddress, secondHostAddress)
  }

  "an action from an unsaved address" - {
    "saves the connection" in appContextRes.use { (context, db) =>
      for {
        (hostWelcome, playerWelcome) <- startableGameFixture(context)
        _ <- performStartGame(startRequest(hostWelcome, playerWelcome), context(secondHostAddress))
        connections <- db.getConnections(hostWelcome.gameId)
      } yield connections.find(_.address == secondHostAddress.address).value.playerId shouldEqual hostWelcome.playerId.pid
    }

    "is sent the update" in appContextRes.use { (context, _) =>
      for {
        (hostWelcome, playerWelcome) <- startableGameFixture(context)
        response <- performStartGame(startRequest(hostWelcome, playerWelcome), context(secondHostAddress))
      } yield response.statuses.keySet shouldEqual Set(hostAddress, secondHostAddress, playerAddress)
    }
  }

  "a gone connection is removed when sending to it" in appContextRes.use { (context, db) =>
    val goneMessaging = new Messaging[IO] {
      override def sendMessage(address: PlayerAddress, message: Message): IO[SendResult] =
        IO.pure(if (address == hostAddress) Gone else Sent)
      override def sendError(address: PlayerAddress, message: Failures): IO[SendResult] = IO.pure(Sent)
    }
    for {
      welcome <- createGameFixture(context)
      _ <- ping(welcome, context(secondHostAddress))
      response <- performJoinGame(joinGameRequest(welcome.gameCode), context(playerAddress))
      _ <- PokerDot.sendResponse(response, goneMessaging, db)
      connections <- db.getConnections(welcome.gameId)
    } yield connections.map(_.address).toSet shouldEqual Set(secondHostAddress.address, playerAddress.address)
  }

  private def createGameFixture(contextBuilder: PlayerAddress => AppContext[IO]): IO[Welcome] = {
    performCreateGame(createGameRequest, contextBuilder(hostAddress), initialSeed).map { response =>
      response.messages.get(hostAddress).value
    }
  }

  private def startableGameFixture(contextBuilder: PlayerAddress => AppContext[IO]): IO[(Welcome, Welcome)] = {
    for {
      hostWelcome <- createGameFixture(contextBuilder)
      joinResponse <- performJoinGame(joinGameRequest(hostWelcome.gameCode), contextBuilder(playerAddress))
    } yield (hostWelcome, joinResponse.messages.get(playerAddress).value)
  }

  private def startRequest(hostWelcome: Welcome, playerWelcome: Welcome): String = {
    startGameRequest(hostWelcome, None, None, None, List(hostWelcome.playerId, playerWelcome.playerId))
  }

  private def ping(welcome: Welcome, context: AppContext[IO]): IO[Response[GameStatus]] = {
    val request = Ping(welcome.gameId, welcome.playerId, welcome.playerKey)
    PokerDot.ping[IO](parseReq(Serialisation.RequestEncoders.encodeRequest(request).noSpaces), context)
  }
}
