package io.adamnfish.pokerdot.integration

import cats.effect.*
import cats.effect.testing.scalatest.AsyncIOSpec
import io.adamnfish.pokerdot.{PokerDot, TestHelpers}
import io.adamnfish.pokerdot.integration.CreateGameIntegrationTest.{
  createGameRequest,
  performCreateGame
}
import io.adamnfish.pokerdot.models.{
  AppContext,
  GameStatus,
  PlayerAddress,
  PlayerId,
  TimerLevel,
  Welcome
}
import org.scalatest.OptionValues
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers
import io.adamnfish.pokerdot.integration.IntegrationComponents.{
  advancePhaseRequest,
  betRequest,
  checkRequest,
  foldRequest
}
import io.adamnfish.pokerdot.integration.JoinGameIntegrationTest.{
  joinGameRequest,
  performJoinGame
}
import io.adamnfish.pokerdot.integration.StartGameIntegrationTest.{
  performStartGame,
  startGameRequest
}
import org.scalactic.source.Position

class GameplayIntegrationTestHeadsUp
    extends AsyncFreeSpec
    with AsyncIOSpec
    with Matchers
    with IntegrationComponents
    with TestHelpers
    with OptionValues {
  val hostAddress = PlayerAddress("host-address")
  val player1Address = PlayerAddress("player-1-address")

  "example heads-up game" in appContextRes.use { (context, db) =>
    for {
      (_, hostWelcome, p1Welcome) <- gameFixture(
        context,
        initialSeed = 0L, // determines deck order
        startingStack = Some(1000),
        initialSmallBlind = Some(5),
        timerConfig = None
      )

      // check initial state
      gameDbOpt1 <- db.getGame(hostWelcome.gameId)
      _ = gameDbOpt1.value should have(
        "button" as 0,
        "inTurn" as Some(hostWelcome.playerId.pid)
      )

      // community: K♦  A♦  Q♠  6♥  J♣
      //   host:    Q♦  7♣
      //   p1:      10♠ 7♦
      // host is dealer and small blind, p1 big blind
      // host is initial player
      // host gas Q♦  7♣ and folds
      _ <- PokerDot.pokerdot(foldRequest(hostWelcome), context(hostAddress))
      // no more actions required
      // advances straight to showdown (since there is only a winner left in the round)
      _ <- PokerDot.pokerdot(
        advancePhaseRequest(hostWelcome),
        context(hostAddress)
      )
      // advance to next round
      _ <- PokerDot.pokerdot(
        advancePhaseRequest(hostWelcome),
        context(hostAddress)
      )

      // players should be reset for the new round correctly
      playerDbsNewRound <- db
        .getPlayers(hostWelcome.gameId)
        .map(playerDbs =>
          playerDbs.map(pdb => (PlayerId(pdb.playerId), pdb)).toMap
        )
      _ = playerDbsNewRound.get(hostWelcome.playerId).value should have(
        "checked" as false,
        "folded" as false,
        "bet" as 10,
        "pot" as 0,
        "blind" as 2
      )
      _ = playerDbsNewRound.get(p1Welcome.playerId).value should have(
        "checked" as false,
        "folded" as false,
        "bet" as 5,
        "pot" as 0,
        "blind" as 1
      )
      // dealer should have moved correctly
      gameDbOpt2 <- db.getGame(hostWelcome.gameId)
      _ = gameDbOpt2.value should have(
        "button" as 1,
        "inTurn" as Some(p1Welcome.playerId.pid)
      )

      // new round
      // player 1 is first to act, and folds
      _ <- PokerDot.pokerdot(foldRequest(p1Welcome), context(player1Address))
    } yield assert(true)
  }

  "checking on big blind should end the phase" in appContextRes.use {
    (context, db) =>
      for {
        (_, hostWelcome, p1Welcome) <- gameFixture(
          context,
          initialSeed = 0L, // determines deck order
          startingStack = Some(1000),
          initialSmallBlind = Some(5),
          timerConfig = None
        )

        // host:    Q♦  7♣
        // p1:      10♠ 7♦
        // host is dealer and small blind, p1 big blind
        // host is initial player, and calls
        _ <- PokerDot
          .pokerdot(betRequest(5, hostWelcome), context(hostAddress))
        // p1 checks as big blind
        _ <- PokerDot
          .pokerdot(checkRequest(p1Welcome), context(player1Address))

        // players have acted and should be marked as checked
        playerDbsNewRound <- db
          .getPlayers(hostWelcome.gameId)
          .map(playerDbs =>
            playerDbs
              .map(pdb => (PlayerId(pdb.playerId), pdb))
              .toMap
          )
        _ = playerDbsNewRound.get(hostWelcome.playerId).value should have(
          "checked" as true,
          "bet" as 10
        )
        _ = playerDbsNewRound.get(p1Welcome.playerId).value should have(
          "checked" as true,
          "bet" as 10
        )

        // both players have acted, no-one should be "in turn"
        gameDbOpt <- db.getGame(hostWelcome.gameId)
      } yield gameDbOpt.value should have(
        "inTurn" as None
      )
  }

  "calling a bet should end the phase" in appContextRes.use { (context, db) =>
    for {
      (_, hostWelcome, p1Welcome) <- gameFixture(
        context,
        initialSeed = 0L, // determines deck order
        startingStack = Some(1000),
        initialSmallBlind = Some(5),
        timerConfig = None
      )

      // host:    Q♦  7♣
      // p1:      10♠ 7♦
      // host is dealer and small blind, p1 big blind
      // host is initial player, and raises
      _ <- PokerDot.pokerdot(betRequest(15, hostWelcome), context(hostAddress))
      // p1 calls
      _ <- PokerDot.pokerdot(betRequest(10, p1Welcome), context(player1Address))
      // players have acted and should be marked as checked
      playerDbsNewRound <- db
        .getPlayers(hostWelcome.gameId)
        .map(playerDbs =>
          playerDbs.map(pdb => (PlayerId(pdb.playerId), pdb)).toMap
        )
      _ = playerDbsNewRound.get(hostWelcome.playerId).value should have(
        "checked" as true,
        "bet" as 20
      )
      _ = playerDbsNewRound.get(p1Welcome.playerId).value should have(
        "checked" as true,
        "bet" as 20
      )

      // both players have acted, no-one should be "in turn"
      gameDbOpt <- db.getGame(hostWelcome.gameId)
    } yield gameDbOpt.value should have(
      "inTurn" as None
    )
  }

  private def gameFixture(
      contextBuilder: PlayerAddress => AppContext[IO],
      initialSeed: Long,
      startingStack: Option[Int],
      initialSmallBlind: Option[Int],
      timerConfig: Option[List[TimerLevel]]
  )(implicit pos: Position): IO[(GameStatus, Welcome, Welcome)] = {
    for {
      hostResponse <- performCreateGame(
        createGameRequest,
        contextBuilder(hostAddress),
        initialSeed
      )
      hostWelcome = hostResponse.messages
        .find { case (address, _) =>
          address == hostAddress
        }
        .map(_._2)
        .value
      gameCode = hostWelcome.gameCode
      p1JoinResponse <- performJoinGame(
        joinGameRequest(gameCode, "player-1"),
        contextBuilder(player1Address)
      )
      p1Welcome = p1JoinResponse.messages.get(player1Address).value
      startRequest = startGameRequest(
        hostWelcome,
        startingStack,
        initialSmallBlind,
        timerConfig,
        List(hostWelcome.playerId, p1Welcome.playerId)
      )
      startResponse <- performStartGame(
        startRequest,
        contextBuilder(hostAddress)
      )
      gameStatus = startResponse.statuses.get(hostAddress).value
    } yield (gameStatus, hostWelcome, p1Welcome)
  }
}
