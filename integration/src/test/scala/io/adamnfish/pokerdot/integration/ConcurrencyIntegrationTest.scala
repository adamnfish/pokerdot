package io.adamnfish.pokerdot.integration

import cats.effect.*
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.syntax.all.*
import io.adamnfish.pokerdot.TestHelpers.parseReq
import io.adamnfish.pokerdot.integration.CreateGameIntegrationTest.{createGameRequest, performCreateGame}
import io.adamnfish.pokerdot.integration.IntegrationComponents.{afterGetPlayers, betRequest}
import io.adamnfish.pokerdot.integration.JoinGameIntegrationTest.{joinGameRequest, performJoinGame}
import io.adamnfish.pokerdot.integration.StartGameIntegrationTest.{performStartGame, startGameRequest}
import io.adamnfish.pokerdot.logic.{Games, Representations}
import io.adamnfish.pokerdot.models.*
import io.adamnfish.pokerdot.{PokerDot, TestHelpers}
import org.scalatest.OptionValues
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers


class ConcurrencyIntegrationTest
    extends AsyncFreeSpec
    with AsyncIOSpec
    with Matchers
    with IntegrationComponents
    with TestHelpers
    with OptionValues {
  val hostAddress = PlayerAddress("host-address")
  val player1Address = PlayerAddress("player-1-address")

  "a double-submitted bet is only applied once" in appContextRes.use { (context, db) =>
    // the race depends on timing, so try it repeatedly
    (1 to 10).toList.traverse_ { _ =>
      for {
        (hostWelcome, _) <- startedHeadsUpGame(context)
        // heads-up, the host is the dealer, posts the small blind (5) and acts first
        request = betRequest(5, hostWelcome)
        results <- (
          PokerDot.pokerdot[IO](request, context(hostAddress)).attempt,
          PokerDot.pokerdot[IO](request, context(hostAddress)).attempt,
        ).parTupled
        playerDbs <- db.getPlayers(hostWelcome.gameId)
        hostDb = playerDbs.find(_.playerId == hostWelcome.playerId.pid).value
        gameDb <- db.getGame(hostWelcome.gameId)
      } yield {
        List(results._1, results._2).count(_.isRight) shouldEqual 1
        hostDb.stack shouldEqual 990
        hostDb.bet shouldEqual 10
        // created at 0, started at 1, one bet
        gameDb.value.revision shouldEqual 2
      }
    }.as(succeed)
  }

  "a write based on a stale read of the game is rejected" in appContextRes.use { (context, db) =>
    for {
      (hostWelcome, _) <- startedHeadsUpGame(context)
      staleGameDb <- db.getGame(hostWelcome.gameId).map(_.value)
      stalePlayerDbs <- db.getPlayers(hostWelcome.gameId)
      _ <- PokerDot.pokerdot[IO](betRequest(5, hostWelcome), context(hostAddress))
      result <- db.writeGameAndPlayers(
        staleGameDb,
        stalePlayerDbs.map(_.copy(stack = 0)),
      ).attempt
      playerDbs <- db.getPlayers(hostWelcome.gameId)
      hostDb = playerDbs.find(_.playerId == hostWelcome.playerId.pid).value
    } yield {
      result match {
        case Left(failures: Failures) =>
          failures.externalFailures.map(_.userMessage) shouldEqual List(
            "someone else changed the game at the same time, please check and try again."
          )
        case other =>
          fail(s"expected the stale write to fail, got $other")
      }
      // the bet survived
      hostDb.stack shouldEqual 990
    }
  }

  "an action does not overwrite a player's address changed after the action read the player" in appContextRes.use { (context, db) =>
    val newHostAddress = PlayerAddress("new-host-address")
    for {
      (hostWelcome, _) <- startedHeadsUpGame(context)
      // simulate a ping from a new address between the bet's read and its write
      concurrentDb = afterGetPlayers(db) { _ =>
        db.updatePlayerAddress(hostWelcome.gameId, hostWelcome.playerId, newHostAddress).void
      }
      _ <- PokerDot.pokerdot[IO](betRequest(5, hostWelcome), context(hostAddress).copy(db = concurrentDb))
      playerDbs <- db.getPlayers(hostWelcome.gameId)
      hostDb = playerDbs.find(_.playerId == hostWelcome.playerId.pid).value
    } yield {
      hostDb.playerAddress shouldEqual newHostAddress.address
      hostDb.stack shouldEqual 990
    }
  }

  "a player cannot be added to a game that started after it was read" in appContextRes.use { (context, db) =>
    for {
      hostResponse <- performCreateGame(createGameRequest, context(hostAddress), 0L)
      hostWelcome = hostResponse.messages.get(hostAddress).value
      p1JoinResponse <- performJoinGame(joinGameRequest(hostWelcome.gameCode, "player-1"), context(player1Address))
      p1Welcome = p1JoinResponse.messages.get(player1Address).value
      unstartedGameDb <- db.getGame(hostWelcome.gameId).map(_.value)
      _ <- performStartGame(
        startGameRequest(hostWelcome, Some(1000), Some(5), None, List(hostWelcome.playerId, p1Welcome.playerId)),
        context(hostAddress)
      )
      latePlayer = Games.newPlayer(hostWelcome.gameId, "late", false, PlayerAddress("late-address"), 0L)
      result <- db.addPlayer(unstartedGameDb, Representations.playerToDb(latePlayer)).attempt
      playerDbs <- db.getPlayers(hostWelcome.gameId)
    } yield {
      result.isLeft shouldEqual true
      playerDbs.map(_.playerId) should not contain latePlayer.playerId.pid
    }
  }

  "a game cannot be created over an existing game" in appContextRes.use { (context, db) =>
    for {
      hostResponse <- performCreateGame(createGameRequest, context(hostAddress), 0L)
      hostWelcome = hostResponse.messages.get(hostAddress).value
      gameDb <- db.getGame(hostWelcome.gameId).map(_.value)
      otherPlayer = Games.newPlayer(hostWelcome.gameId, "other", true, PlayerAddress("other-address"), 0L)
      result <- db.createGame(gameDb.copy(gameName = "overwritten", revision = 0), Representations.playerToDb(otherPlayer)).attempt
      afterGameDb <- db.getGame(hostWelcome.gameId).map(_.value)
      playerDbs <- db.getPlayers(hostWelcome.gameId)
    } yield {
      result.isLeft shouldEqual true
      afterGameDb.gameName shouldEqual gameDb.gameName
      playerDbs.map(_.playerId) should not contain otherPlayer.playerId.pid
    }
  }

  "a write that includes a missing player changes nothing" in appContextRes.use { (context, db) =>
    for {
      (hostWelcome, _) <- startedHeadsUpGame(context)
      gameDb <- db.getGame(hostWelcome.gameId).map(_.value)
      playerDbs <- db.getPlayers(hostWelcome.gameId)
      missingPlayer = Representations.playerToDb(
        Games.newPlayer(hostWelcome.gameId, "missing", false, PlayerAddress("missing-address"), 0L)
      )
      result <- db.writeGameAndPlayers(
        gameDb.copy(button = 1),
        playerDbs.map(_.copy(stack = 0)) :+ missingPlayer,
      ).attempt
      afterGameDb <- db.getGame(hostWelcome.gameId).map(_.value)
      afterPlayerDbs <- db.getPlayers(hostWelcome.gameId)
    } yield {
      result.isLeft shouldEqual true
      afterGameDb shouldEqual gameDb
      afterPlayerDbs.map(_.stack) should not contain 0
      afterPlayerDbs.map(_.playerId) should not contain missingPlayer.playerId
    }
  }

  private def startedHeadsUpGame(contextBuilder: PlayerAddress => AppContext[IO]): IO[(Welcome, Welcome)] = {
    for {
      hostResponse <- performCreateGame(createGameRequest, contextBuilder(hostAddress), 0L)
      hostWelcome = hostResponse.messages.get(hostAddress).value
      p1JoinResponse <- performJoinGame(
        joinGameRequest(hostWelcome.gameCode, "player-1"),
        contextBuilder(player1Address)
      )
      p1Welcome = p1JoinResponse.messages.get(player1Address).value
      _ <- performStartGame(
        startGameRequest(hostWelcome, Some(1000), Some(5), None, List(hostWelcome.playerId, p1Welcome.playerId)),
        contextBuilder(hostAddress)
      )
    } yield (hostWelcome, p1Welcome)
  }
}
