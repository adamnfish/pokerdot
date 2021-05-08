package io.adamnfish.pokerdot.integration

import io.adamnfish.pokerdot.{PokerDot, TestHelpers}
import io.adamnfish.pokerdot.integration.CreateGameIntegrationTest.{createGameRequest, performCreateGame}
import io.adamnfish.pokerdot.models.{AppContext, Attempt, GameStatus, PlayerAddress, PlayerId, TimerLevel, Welcome}
import org.scalatest.OptionValues
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import io.adamnfish.pokerdot.integration.IntegrationComponents.{advancePhaseRequest, betRequest, checkRequest, foldRequest}
import io.adamnfish.pokerdot.integration.JoinGameIntegrationTest.{joinGameRequest, performJoinGame}
import io.adamnfish.pokerdot.integration.StartGameIntegrationTest.{performStartGame, startGameRequest}
import org.scalactic.source.Position


class GameplayIntegrationTestHeadsUp extends AnyFreeSpec with Matchers with IntegrationComponents with TestHelpers with OptionValues {
  val hostAddress = PlayerAddress("host-address")
  val player1Address = PlayerAddress("player-1-address")

  "example heads-up game" in withAppContext { (context, db) =>
    val (_, hostWelcome, p1Welcome) = gameFixture(context,
      initialSeed = 0L, // determines deck order
      startingStack = Some(1000),
      initialSmallBlind = Some(5),
      timerConfig = None,
    ).value()

    // check initial state
    db.getGame(hostWelcome.gameId).value().value should have(
      "button" as 0,
      "inTurn" as Some(hostWelcome.playerId.pid),
    )
    // community: K♦  A♦  Q♠  6♥  J♣
    //   host:    Q♦  7♣
    //   p1:      10♠ 7♦
    // host is dealer and small blind, p1 big blind
    // host is initial player
    // host gas Q♦  7♣ and folds
    PokerDot.pokerdot(foldRequest(hostWelcome), context(hostAddress)).value()
    // no more actions required
    // advances to flop
    PokerDot.pokerdot(advancePhaseRequest(hostWelcome), context(hostAddress)).value()
    // advances to turn
    PokerDot.pokerdot(advancePhaseRequest(hostWelcome), context(hostAddress)).value()
    // advances to river
    PokerDot.pokerdot(advancePhaseRequest(hostWelcome), context(hostAddress)).value()
    // advance to showdown
    PokerDot.pokerdot(advancePhaseRequest(hostWelcome), context(hostAddress)).value()
    // advance to next round
    PokerDot.pokerdot(advancePhaseRequest(hostWelcome), context(hostAddress)).value()

    // players should be reset for the new round correctly
    val playerDbsNewRound = db.getPlayers(hostWelcome.gameId).value().map(pdb => (PlayerId(pdb.playerId), pdb)).toMap
    playerDbsNewRound.get(hostWelcome.playerId).value should have(
      "checked" as false,
      "folded" as false,
      "bet" as 10,
      "pot" as 0,
      "blind" as 2,
    )
    playerDbsNewRound.get(p1Welcome.playerId).value should have(
      "checked" as false,
      "folded" as false,
      "bet" as 5,
      "pot" as 0,
      "blind" as 1,
    )
    // dealer should have moved correctly
    db.getGame(hostWelcome.gameId).value().value should have(
      "button" as 1,
      "inTurn" as Some(p1Welcome.playerId.pid),
    )

    // new round
    // player 1 is first to act, and folds
    PokerDot.pokerdot(foldRequest(p1Welcome), context(player1Address)).value()
  }

  "checking on big blind should end the phase" in withAppContext { (context, db) =>
    val (_, hostWelcome, p1Welcome) = gameFixture(context,
      initialSeed = 0L, // determines deck order
      startingStack = Some(1000),
      initialSmallBlind = Some(5),
      timerConfig = None,
    ).value()

    // host:    Q♦  7♣
    // p1:      10♠ 7♦
    // host is dealer and small blind, p1 big blind
    // host is initial player, and calls
    PokerDot.pokerdot(betRequest(5, hostWelcome), context(hostAddress)).value()
    // p1 checks as big blind
    PokerDot.pokerdot(checkRequest(p1Welcome), context(player1Address)).value()

    // players have acted and should be marked as checked
    val playerDbsNewRound = db.getPlayers(hostWelcome.gameId).value().map(pdb => (PlayerId(pdb.playerId), pdb)).toMap
    playerDbsNewRound.get(hostWelcome.playerId).value should have(
      "checked" as true,
      "bet" as 10,
    )
    playerDbsNewRound.get(p1Welcome.playerId).value should have(
      "checked" as true,
      "bet" as 10,
    )

    // both players have acted, no-one should be "in turn"
    db.getGame(hostWelcome.gameId).value().value should have(
      "inTurn" as None,
    )
  }

  "calling a bet should end the phase" in withAppContext { (context, db) =>
    val (_, hostWelcome, p1Welcome) = gameFixture(context,
      initialSeed = 0L, // determines deck order
      startingStack = Some(1000),
      initialSmallBlind = Some(5),
      timerConfig = None,
    ).value()

    // host:    Q♦  7♣
    // p1:      10♠ 7♦
    // host is dealer and small blind, p1 big blind
    // host is initial player, and raises
    PokerDot.pokerdot(betRequest(15, hostWelcome), context(hostAddress)).value()
    // p1 calls
    PokerDot.pokerdot(betRequest(10, p1Welcome), context(player1Address)).value()
    // players have acted and should be marked as checked
    val playerDbsNewRound = db.getPlayers(hostWelcome.gameId).value().map(pdb => (PlayerId(pdb.playerId), pdb)).toMap
    playerDbsNewRound.get(hostWelcome.playerId).value should have(
      "checked" as true,
      "bet" as 20,
    )
    playerDbsNewRound.get(p1Welcome.playerId).value should have(
      "checked" as true,
      "bet" as 20,
    )

    // both players have acted, no-one should be "in turn"
    db.getGame(hostWelcome.gameId).value().value should have(
      "inTurn" as None,
    )
  }

  private def gameFixture(
    contextBuilder: PlayerAddress => AppContext,
    initialSeed: Long,
    startingStack: Option[Int],
    initialSmallBlind: Option[Int],
    timerConfig: Option[List[TimerLevel]],
  )(implicit pos: Position): Attempt[(GameStatus, Welcome, Welcome)] = {
    for {
      hostResponse <- performCreateGame(createGameRequest, contextBuilder(hostAddress), initialSeed)
      hostWelcome = hostResponse.messages.find { case (address, _) =>
        address == hostAddress
      }.map(_._2).value
      gameCode = hostWelcome.gameCode
      p1JoinResponse <- performJoinGame(joinGameRequest(gameCode, "player-1"), contextBuilder(player1Address))
      p1Welcome = p1JoinResponse.messages.get(player1Address).value
      startRequest = startGameRequest(hostWelcome, startingStack, initialSmallBlind, timerConfig,
        List(hostWelcome.playerId, p1Welcome.playerId)
      )
      startResponse <- performStartGame(startRequest, contextBuilder(hostAddress))
      gameStatus = startResponse.statuses.get(hostAddress).value
    } yield (gameStatus, hostWelcome, p1Welcome)
  }
}
