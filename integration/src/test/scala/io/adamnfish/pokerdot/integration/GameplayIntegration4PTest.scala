package io.adamnfish.pokerdot.integration

import io.adamnfish.pokerdot.TestHelpers.parseReq
import io.adamnfish.pokerdot.integration.CreateGameIntegrationTest.{createGameRequest, performCreateGame}
import io.adamnfish.pokerdot.integration.IntegrationComponents.{advancePhaseRequest, betRequest, checkRequest, foldRequest}
import io.adamnfish.pokerdot.integration.JoinGameIntegrationTest.{joinGameRequest, performJoinGame}
import io.adamnfish.pokerdot.integration.StartGameIntegrationTest.{performStartGame, startGameRequest}
import io.adamnfish.pokerdot.logic.Cards.RichRank
import io.adamnfish.pokerdot.models._
import io.adamnfish.pokerdot.{PokerDot, RunningTestClock, TestHelpers}
import org.scalactic.source.Position
import org.scalatest.OptionValues
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers


class GameplayIntegration4PTest extends AnyFreeSpec with Matchers with IntegrationComponents with TestHelpers with OptionValues {
  val hostAddress = PlayerAddress("host-address")
  val player1Address = PlayerAddress("player-1-address")
  val player2Address = PlayerAddress("player-2-address")
  val player3Address = PlayerAddress("player-3-address")

  "poker gameplay works" - {
    "for an example game" in withAppContext { (context, db, testClock) =>
      implicit val clock: RunningTestClock = testClock

      val (_, hostWelcome, p1Welcome, p2Welcome, p3Welcome) = gameFixture(context,
        initialSeed = 0L, // determines deck order
        startingStack = Some(1000),
        initialSmallBlind = Some(5),
        timerConfig = None,
      ).value()

      val startGameLog = db.getFullGameLog(hostWelcome.gameId).value()
      startGameLog.head should have(
        "gid" as hostWelcome.gameId.gid,
        "e" as NP("p"),
      )
      startGameLog(1).e shouldBe a[NR]
      startGameLog(2) should have(
        "gid" as hostWelcome.gameId.gid,
        "e" as GS(List(hostWelcome.playerId.pid, p1Welcome.playerId.pid, p2Welcome.playerId.pid, p3Welcome.playerId.pid)),
      )

      // community: K♦  A♦  Q♠  6♥  J♣
      //   host:    Q♦  7♣
      //   p1:      10♠ 7♦
      //   p2:      Q♣  J♥
      //   p3:      7♠  6♠
      // host is dealer, p1 small blind, p2 big blind, p3 first to act
      // p3 is initial player (left of dealer small blind and big blind)
      // p3 gas 7♠ 6♠ and folds
      PokerDot.pokerdot(foldRequest(p3Welcome), context(player3Address)).tick().value()
      db.getPhaseGameLog(hostWelcome.gameId).value().head.e shouldEqual F(p3Welcome.playerId.pid)
      // host has Q♦ 7♣ and calls
      PokerDot.pokerdot(betRequest(10, hostWelcome), context(hostAddress)).tick().value()
      db.getPhaseGameLog(hostWelcome.gameId).value().head.e shouldEqual B(hostWelcome.playerId.pid, 10)
      // p1 has 10♠ 7♦ and calls from small blind
      PokerDot.pokerdot(betRequest(5, p1Welcome), context(player1Address)).tick().value()
      db.getPhaseGameLog(hostWelcome.gameId).value().head.e shouldEqual B(p1Welcome.playerId.pid, 5)
      // p2 has J♥ Q♣ and checks from big blind
      PokerDot.pokerdot(checkRequest(p2Welcome), context(player2Address)).tick().value()
      db.getPhaseGameLog(hostWelcome.gameId).value().head.e shouldEqual C(p2Welcome.playerId.pid)

      // phase is now complete

      val playerDbsPreFlop = db.getPlayers(hostWelcome.gameId).value().map(pdb => (PlayerId(pdb.playerId), pdb)).toMap
      playerDbsPreFlop.get(hostWelcome.playerId).value should have(
        "checked" as true,
        "bet" as 10,
        "pot" as 0,
      )
      playerDbsPreFlop.get(p1Welcome.playerId).value should have(
        "checked" as true,
        "bet" as 10,
        "pot" as 0,
      )
      playerDbsPreFlop.get(p2Welcome.playerId).value should have(
        "checked" as true,
        "bet" as 10,
        "pot" as 0,
      )
      playerDbsPreFlop.get(p3Welcome.playerId).value should have(
        "folded" as true,
        "bet" as 0,
        "pot" as 0,
      )
      PokerDot.pokerdot(advancePhaseRequest(hostWelcome), context(hostAddress)).tick().value()
      db.getPhaseGameLog(hostWelcome.gameId).value().head.e shouldEqual NP("f")

      // community cards K♦ A♦ Q♠ are now visible
      // p1 is first to act, and checks
      PokerDot.pokerdot(checkRequest(p1Welcome), context(player1Address)).tick().value()
      // p2 bets
      PokerDot.pokerdot(betRequest(10, p2Welcome), context(player2Address)).tick().value()
      // p3 has folded
      // host calls
      PokerDot.pokerdot(betRequest(10, hostWelcome), context(hostAddress)).tick().value()
      // p1 needs to react to the bet, and calls with a straight draw
      PokerDot.pokerdot(betRequest(10, p1Welcome), context(player1Address)).tick().value()
      // phase is complete
      val playerDbsFlop = db.getPlayers(hostWelcome.gameId).value().map(pdb => (PlayerId(pdb.playerId), pdb)).toMap
      playerDbsFlop.get(hostWelcome.playerId).value should have(
        "checked" as true,
        "bet" as 10,
        "pot" as 10,
      )
      playerDbsFlop.get(p1Welcome.playerId).value should have(
        "checked" as true,
        "bet" as 10,
        "pot" as 10,
      )
      playerDbsFlop.get(p2Welcome.playerId).value should have(
        "checked" as true,
        "bet" as 10,
        "pot" as 10,
      )
      playerDbsFlop.get(p3Welcome.playerId).value should have(
        "folded" as true,
        "bet" as 0,
        "bet" as 0,
      )
      PokerDot.pokerdot(advancePhaseRequest(hostWelcome), context(hostAddress)).tick().value()

      // community cards K♦ A♦ Q♠ 6♥ are now visible
      // players are cautious about overcards and all check
      PokerDot.pokerdot(checkRequest(p1Welcome), context(player1Address)).tick().value()
      PokerDot.pokerdot(checkRequest(p2Welcome), context(player2Address)).tick().value()
      PokerDot.pokerdot(checkRequest(hostWelcome), context(hostAddress)).tick().value()
      // phase is complete
      val playerDbsTurn = db.getPlayers(hostWelcome.gameId).value().map(pdb => (PlayerId(pdb.playerId), pdb)).toMap
      playerDbsTurn.get(hostWelcome.playerId).value should have(
        "checked" as true,
        "bet" as 0,
        "pot" as 20,
      )
      playerDbsTurn.get(p1Welcome.playerId).value should have(
        "checked" as true,
        "bet" as 0,
        "pot" as 20,
      )
      playerDbsTurn.get(p2Welcome.playerId).value should have(
        "checked" as true,
        "bet" as 0,
        "pot" as 20,
      )
      playerDbsTurn.get(p3Welcome.playerId).value should have(
        "folded" as true,
        "bet" as 0,
        "bet" as 0,
      )
      PokerDot.pokerdot(advancePhaseRequest(hostWelcome), context(hostAddress)).tick().value()

      // all community cards now visible K♦ A♦ Q♠ 6♥ J♣
      // p1 has lucked a straight, and bets
      PokerDot.pokerdot(betRequest(50, p1Welcome), context(player1Address)).tick().value()
      // p2 has two-pair, decides to call
      PokerDot.pokerdot(betRequest(50, p2Welcome), context(player2Address)).tick().value()
      // host only has pair of queens and will let these two fight it out
      PokerDot.pokerdot(foldRequest(hostWelcome), context(hostAddress)).tick().value()
      // phase is complete
      val playerDbsRiver = db.getPlayers(hostWelcome.gameId).value().map(pdb => (PlayerId(pdb.playerId), pdb)).toMap
      playerDbsRiver.get(hostWelcome.playerId).value should have(
        "folded" as true,
        "bet" as 0,
        "pot" as 20,
      )
      playerDbsRiver.get(p1Welcome.playerId).value should have(
        "checked" as true,
        "bet" as 50,
        "pot" as 20,
      )
      playerDbsRiver.get(p2Welcome.playerId).value should have(
        "checked" as true,
        "bet" as 50,
        "pot" as 20,
      )
      playerDbsRiver.get(p3Welcome.playerId).value should have(
        "folded" as true,
        "bet" as 0,
        "pot" as 0,
      )

      val response = PokerDot.advancePhase(parseReq(advancePhaseRequest(hostWelcome)), context(hostAddress)).tick().value()

      // pots are preserved at this stage to help the UI show how the game is changed by the result
      val playerDbsShowdown = db.getPlayers(hostWelcome.gameId).value().map(pdb => (PlayerId(pdb.playerId), pdb)).toMap
      playerDbsShowdown.get(hostWelcome.playerId).value should have(
        "stack" as 980,
        "bet" as 0,
        "pot" as 20,
      )
      playerDbsShowdown.get(p1Welcome.playerId).value should have(
        "stack" as 1090,
        "bet" as 0,
        "pot" as 70,
      )
      playerDbsShowdown.get(p2Welcome.playerId).value should have(
        "stack" as 930,
        "bet" as 0,
        "pot" as 70,
      )
      playerDbsShowdown.get(p3Welcome.playerId).value should have(
        "stack" as 1000,
        "bet" as 0,
        "pot" as 0,
      )
      val roundWinnings = response.messages.get(hostAddress).value.asInstanceOf[RoundWinnings]
      // only one player wins, with a straight
      roundWinnings.players.toSet shouldEqual Set(
        // folded players do not show up (and are commented out here)
        // PlayerWinnings(hostWelcome.playerId, Pair(Queen of Spades, Queen of Diamonds, Ace of Diamonds, King of Diamonds, Jack of Clubs), 0),
        PlayerWinnings(p1Welcome.playerId, Some(Straight(Ace of Diamonds, King of Diamonds, Queen of Spades, Jack of Clubs, Ten of Spades)), Hole(Ten of Spades, Seven of Diamonds), 160),
        PlayerWinnings(p2Welcome.playerId, Some(TwoPair(Queen of Spades, Queen of Clubs, Jack of Hearts, Jack of Clubs, Ace of Diamonds)), Hole(Jack of Hearts, Queen of Clubs), 0),
        // PlayerWinnings(p3Welcome.playerId, Pair(Six of Spades, Six of Hearts, Ace of Diamonds, King of Diamonds, Queen of Spades), 0),
      )
      // a single pot between players 1 and 2, with player 1 winning
      roundWinnings.pots shouldEqual List(
        PotWinnings(160, Set(p1Welcome.playerId, p2Welcome.playerId), Set(p1Welcome.playerId))
      )

      // advance to next round
      PokerDot.advancePhase(parseReq(advancePhaseRequest(hostWelcome)), context(hostAddress)).tick().value()

      // players should be reset for the new round
      val playerDbsNewRound = db.getPlayers(hostWelcome.gameId).value().map(pdb => (PlayerId(pdb.playerId), pdb)).toMap
      playerDbsNewRound.get(hostWelcome.playerId).value should have(
        "stack" as 980,
        "checked" as false,
        "folded" as false,
        "bet" as 0,
        "pot" as 0,
        "blind" as 0,
      )
      playerDbsNewRound.get(p1Welcome.playerId).value should have(
        "stack" as 1090,
        "checked" as false,
        "folded" as false,
        "bet" as 0,
        "pot" as 0,
        "blind" as 0,
      )
      playerDbsNewRound.get(p2Welcome.playerId).value should have(
        "stack" as 925, // small blind paid out as well as prev round's result
        "checked" as false,
        "folded" as false,
        "bet" as 5,
        "pot" as 0,
        "blind" as 1,
      )
      playerDbsNewRound.get(p3Welcome.playerId).value should have(
        "stack" as 990, // big blind paid out as well as prev round's result
        "checked" as false,
        "folded" as false,
        "bet" as 10,
        "pot" as 0,
        "blind" as 2,
      )
      // dealer and active player should have moved correctly
      val gameDb = db.getGame(hostWelcome.gameId).value().value
      gameDb should have(
        "button" as 1,
        "inTurn" as Some(hostWelcome.playerId.pid),
      )

      // check the game log has been persisted correctly
      val gameLog = db.getFullGameLog(hostWelcome.gameId).value()
      gameLog.head should have(
        "gid" as hostWelcome.gameId.gid,
        "e" as NP("p"),
      )
      gameLog.tail.head should have(
        "gid" as hostWelcome.gameId.gid,
        "e" as NR(
          gameDb.seed, gameDb.button, Some(gameDb.smallBlind),
          Some(p2Welcome.playerId.pid), p3Welcome.playerId.pid,
          List(980, 1090, 930, 1000) // Note: this is total player stacks (including blinds)
        )
      )

      // the phase game log should only contain the new phase event, after the new round
      val phaseLog = db.getPhaseGameLog(hostWelcome.gameId).value()
      phaseLog.length shouldEqual 1
      phaseLog.head should have(
        "gid" as hostWelcome.gameId.gid,
        "e" as NP("p"), // pre-flop
      )

      // TODO: check the full game log has been persisted here?
      // check the log has all the events we'd expect, without getting into too much detail
      val finalGameLog = db.getFullGameLog(hostWelcome.gameId).value()
      val allGameEvents = finalGameLog.map(_.e.getClass.getSimpleName)
      allGameEvents shouldEqual List(
        "NP", "NR", "NP", "F", "B", "B", "NP", "C", "C", "C", "NP", "B", "B", "B", "C", "NP", "C", "B", "B", "F", "NP", "NR", "GS"
      )
    }
  }

  "invalid requests" - {
    "when it isn't the player's turn" - {
      "cannot fold" ignore {}
      "cannot bet" ignore {}
      "cannot check" ignore {}
    }

    "bet cannot exceed stack" ignore {}
  }

  private def gameFixture(
    contextBuilder: PlayerAddress => AppContext,
    initialSeed: Long,
    startingStack: Option[Int],
    initialSmallBlind: Option[Int],
    timerConfig: Option[List[TimerLevel]],
  )(implicit pos: Position, testClock: RunningTestClock): Attempt[(GameStatus, Welcome, Welcome, Welcome, Welcome)] = {
    for {
      hostResponse <- performCreateGame(createGameRequest, contextBuilder(hostAddress), initialSeed)
      hostWelcome = hostResponse.messages.find { case (address, _) =>
        address == hostAddress
      }.map(_._2).value
      gameCode = hostWelcome.gameCode
      p1JoinResponse <- performJoinGame(joinGameRequest(gameCode, "player-1"), contextBuilder(player1Address))
      _ <- testClock.tick()
      p1Welcome = p1JoinResponse.messages.get(player1Address).value
      p2JoinResponse <- performJoinGame(joinGameRequest(gameCode, "player-2"), contextBuilder(player2Address))
      _ <- testClock.tick()
      p2Welcome = p2JoinResponse.messages.get(player2Address).value
      p3JoinResponse <- performJoinGame(joinGameRequest(gameCode, "player-3"), contextBuilder(player3Address))
      _ <- testClock.tick()
      p3Welcome = p3JoinResponse.messages.get(player3Address).value
      startRequest = startGameRequest(hostWelcome, startingStack, initialSmallBlind, timerConfig,
        List(hostWelcome.playerId, p1Welcome.playerId, p2Welcome.playerId, p3Welcome.playerId)
      )
      startResponse <- performStartGame(startRequest, contextBuilder(hostAddress))
      _ <- testClock.tick()
      gameStatus = startResponse.statuses.get(hostAddress).value
    } yield (gameStatus, hostWelcome, p1Welcome, p2Welcome, p3Welcome)
  }
}
