package io.adamnfish.pokerdot.integration

import io.adamnfish.pokerdot.TestHelpers.parseReq
import io.adamnfish.pokerdot.integration.CreateGameIntegrationTest.{createGameRequest, performCreateGame}
import io.adamnfish.pokerdot.integration.IntegrationComponents.{advancePhaseRequest, betRequest, checkRequest, foldRequest}
import io.adamnfish.pokerdot.integration.JoinGameIntegrationTest.{joinGameRequest, performJoinGame}
import io.adamnfish.pokerdot.integration.StartGameIntegrationTest.{performStartGame, startGameRequest}
import io.adamnfish.pokerdot.logic.Cards.RichRank
import io.adamnfish.pokerdot.models.Serialisation.RequestEncoders.encodeRequest
import io.adamnfish.pokerdot.models.*
import io.adamnfish.pokerdot.{PokerDot, TestHelpers}
import cats.effect.*
import cats.effect.testing.scalatest.AsyncIOSpec
import org.scalactic.source.Position
import org.scalatest.OptionValues
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers


class GameplayIntegration4PTest extends AsyncFreeSpec with AsyncIOSpec with Matchers with IntegrationComponents with TestHelpers with OptionValues {
  val hostAddress = PlayerAddress("host-address")
  val player1Address = PlayerAddress("player-1-address")
  val player2Address = PlayerAddress("player-2-address")
  val player3Address = PlayerAddress("player-3-address")

  "poker gameplay works" - {
    "for an example game" in appContextRes.use { (context, db) =>
      for {
        (_, hostWelcome, p1Welcome, p2Welcome, p3Welcome) <- gameFixture(context,
          initialSeed = 0L, // determines deck order
          startingStack = Some(1000),
          initialSmallBlind = Some(5),
          timerConfig = None,
        )
        // community: K♦  A♦  Q♠  6♥  J♣
        //   host:    Q♦  7♣
        //   p1:      10♠ 7♦
        //   p2:      Q♣  J♥
        //   p3:      7♠  6♠
        // host is dealer, p1 small blind, p2 big blind, p3 first to act
        // p3 is initial player (left of dealer small blind and big blind)
        // p3 gas 7♠ 6♠ and folds
        _ <- PokerDot.pokerdot(foldRequest(p3Welcome), context(player3Address))
        // host has Q♦ 7♣ and calls
        _ <- PokerDot.pokerdot(betRequest(10, hostWelcome), context(hostAddress))
        // p1 has 10♠ 7♦ and calls from small blind
        _ <- PokerDot.pokerdot(betRequest(5, p1Welcome), context(player1Address))
        // p2 has J♥ Q♣ and checks from big blind
        _ <- PokerDot.pokerdot(checkRequest(p2Welcome), context(player2Address))
        // phase is now complete
        // TODO: check database for player states here
        playerDbsPreFlop <- db.getPlayers(hostWelcome.gameId).map(playerDbs =>
          playerDbs.map(pdb => (PlayerId(pdb.playerId), pdb)).toMap
        )
        _ = playerDbsPreFlop.get(hostWelcome.playerId).value should have(
          "checked" as true,
          "bet" as 10,
          "pot" as 0,
        )
        _ = playerDbsPreFlop.get(p1Welcome.playerId).value should have(
          "checked" as true,
          "bet" as 10,
          "pot" as 0,
        )
        _ = playerDbsPreFlop.get(p2Welcome.playerId).value should have(
          "checked" as true,
          "bet" as 10,
          "pot" as 0,
        )
        _ = playerDbsPreFlop.get(p3Welcome.playerId).value should have(
          "folded" as true,
          "bet" as 0,
          "pot" as 0,
        )
        _ <- PokerDot.pokerdot(advancePhaseRequest(hostWelcome), context(hostAddress))

        // community cards K♦ A♦ Q♠ are now visible
        // p1 is first to act, and checks
        _ <- PokerDot.pokerdot(checkRequest(p1Welcome), context(player1Address))
        // p2 bets
        _ <- PokerDot.pokerdot(betRequest(10, p2Welcome), context(player2Address))
        // p3 has folded
        // host calls
        _ <- PokerDot.pokerdot(betRequest(10, hostWelcome), context(hostAddress))
        // p1 needs to react to the bet, and calls with a straight draw
        _ <- PokerDot.pokerdot(betRequest(10, p1Welcome), context(player1Address))
        // phase is complete
        playerDbsFlop <- db.getPlayers(hostWelcome.gameId).map(playerDbs =>
          playerDbs.map(pdb => (PlayerId(pdb.playerId), pdb)).toMap
        )
        _ = playerDbsFlop.get(hostWelcome.playerId).value should have(
          "checked" as true,
          "bet" as 10,
          "pot" as 10,
        )
        _ = playerDbsFlop.get(p1Welcome.playerId).value should have(
          "checked" as true,
          "bet" as 10,
          "pot" as 10,
        )
        _ = playerDbsFlop.get(p2Welcome.playerId).value should have(
          "checked" as true,
          "bet" as 10,
          "pot" as 10,
        )
        _ = playerDbsFlop.get(p3Welcome.playerId).value should have(
          "folded" as true,
          "bet" as 0,
          "bet" as 0,
        )
        _ <- PokerDot.pokerdot(advancePhaseRequest(hostWelcome), context(hostAddress))

        // community cards K♦ A♦ Q♠ 6♥ are now visible
        // players are cautious about overcards and all check
        _ <- PokerDot.pokerdot(checkRequest(p1Welcome), context(player1Address))
        _ <- PokerDot.pokerdot(checkRequest(p2Welcome), context(player2Address))
        _ <- PokerDot.pokerdot(checkRequest(hostWelcome), context(hostAddress))
        // phase is complete
        playerDbsTurn <- db.getPlayers(hostWelcome.gameId).map(playerDbs =>
          playerDbs.map(pdb => (PlayerId(pdb.playerId), pdb)).toMap
        )
        _ = playerDbsTurn.get(hostWelcome.playerId).value should have(
          "checked" as true,
          "bet" as 0,
          "pot" as 20,
        )
        _ = playerDbsTurn.get(p1Welcome.playerId).value should have(
          "checked" as true,
          "bet" as 0,
          "pot" as 20,
        )
        _ = playerDbsTurn.get(p2Welcome.playerId).value should have(
          "checked" as true,
          "bet" as 0,
          "pot" as 20,
        )
        _ = playerDbsTurn.get(p3Welcome.playerId).value should have(
          "folded" as true,
          "bet" as 0,
          "bet" as 0,
        )
        _ <- PokerDot.pokerdot(advancePhaseRequest(hostWelcome), context(hostAddress))

        // all community cards now visible K♦ A♦ Q♠ 6♥ J♣
        // p1 has lucked a straight, and bets
        _ <- PokerDot.pokerdot(betRequest(50, p1Welcome), context(player1Address))
        // p2 has two-pair, decides to call
        _ <- PokerDot.pokerdot(betRequest(50, p2Welcome), context(player2Address))
        // host only has a pair of queens and will let these two fight it out
        _ <- PokerDot.pokerdot(foldRequest(hostWelcome), context(hostAddress))
        // phase is complete
        playerDbsRiver <- db.getPlayers(hostWelcome.gameId).map(playerDbs =>
          playerDbs.map(pdb => (PlayerId(pdb.playerId), pdb)).toMap
        )
        _ = playerDbsRiver.get(hostWelcome.playerId).value should have(
          "folded" as true,
          "bet" as 0,
          "pot" as 20,
        )
        _ = playerDbsRiver.get(p1Welcome.playerId).value should have(
          "checked" as true,
          "bet" as 50,
          "pot" as 20,
        )
        _ = playerDbsRiver.get(p2Welcome.playerId).value should have(
          "checked" as true,
          "bet" as 50,
          "pot" as 20,
        )
        _ = playerDbsRiver.get(p3Welcome.playerId).value should have(
          "folded" as true,
          "bet" as 0,
          "pot" as 0,
        )

        response <- PokerDot.advancePhase(parseReq(advancePhaseRequest(hostWelcome)), context(hostAddress))

        // pots are preserved at this stage to help the UI show how the game is changed by the result
        playerDbsShowdown <- db.getPlayers(hostWelcome.gameId).map(playerDbs =>
          playerDbs.map(pdb => (PlayerId(pdb.playerId), pdb)).toMap
        )
        _ = playerDbsShowdown.get(hostWelcome.playerId).value should have(
          "stack" as 980,
          "bet" as 0,
          "pot" as 20,
        )
        _ = playerDbsShowdown.get(p1Welcome.playerId).value should have(
          "stack" as 1090,
          "bet" as 0,
          "pot" as 70,
        )
        _ = playerDbsShowdown.get(p2Welcome.playerId).value should have(
          "stack" as 930,
          "bet" as 0,
          "pot" as 70,
        )
        _ = playerDbsShowdown.get(p3Welcome.playerId).value should have(
          "stack" as 1000,
          "bet" as 0,
          "pot" as 0,
        )
        roundWinnings = response.messages.get(hostAddress).value.asInstanceOf[RoundWinnings]
        // only one player wins, with a straight
        _ = roundWinnings.players.toSet shouldEqual Set(
          // folded players do not show up (and are commented out here)
          // PlayerWinnings(hostWelcome.playerId, Pair(Queen of Spades, Queen of Diamonds, Ace of Diamonds, King of Diamonds, Jack of Clubs), 0),
          PlayerWinnings(p1Welcome.playerId, Some(Straight(Ace of Diamonds, King of Diamonds, Queen of Spades, Jack of Clubs, Ten of Spades)), Hole(Ten of Spades, Seven of Diamonds), 160),
          PlayerWinnings(p2Welcome.playerId, Some(TwoPair(Queen of Spades, Queen of Clubs, Jack of Hearts, Jack of Clubs, Ace of Diamonds)), Hole(Jack of Hearts, Queen of Clubs), 0),
          // PlayerWinnings(p3Welcome.playerId, Pair(Six of Spades, Six of Hearts, Ace of Diamonds, King of Diamonds, Queen of Spades), 0),
        )
        // a single pot between players 1 and 2, with player 1 winning
        _ = roundWinnings.pots shouldEqual List(
          PotWinnings(160, Set(p1Welcome.playerId, p2Welcome.playerId), Set(p1Welcome.playerId))
        )

        // advance to next round
        _ <- PokerDot.advancePhase(parseReq(advancePhaseRequest(hostWelcome)), context(hostAddress))

        // players should be reset for the new round
        playerDbsNewRound <- db.getPlayers(hostWelcome.gameId).map(playerDbs =>
          playerDbs.map(pdb => (PlayerId(pdb.playerId), pdb)).toMap
        )
        _ = playerDbsNewRound.get(hostWelcome.playerId).value should have(
          "stack" as 980,
          "checked" as false,
          "folded" as false,
          "bet" as 0,
          "pot" as 0,
          "blind" as 0,
        )
        _ = playerDbsNewRound.get(p1Welcome.playerId).value should have(
          "stack" as 1090,
          "checked" as false,
          "folded" as false,
          "bet" as 0,
          "pot" as 0,
          "blind" as 0,
        )
        _ = playerDbsNewRound.get(p2Welcome.playerId).value should have(
          "stack" as 925, // small blind paid out as well as prev round's result
          "checked" as false,
          "folded" as false,
          "bet" as 5,
          "pot" as 0,
          "blind" as 1,
        )
        _ = playerDbsNewRound.get(p3Welcome.playerId).value should have(
          "stack" as 990, // big blind paid out as well as prev round's result
          "checked" as false,
          "folded" as false,
          "bet" as 10,
          "pot" as 0,
          "blind" as 2,
        )
        // dealer and active player should have moved correctly
        finalGameOpt <- db.getGame(hostWelcome.gameId)
      } yield finalGameOpt.value should have(
        "button" as 1,
        "inTurn" as Some(hostWelcome.playerId.pid),
      )
    }
  }

  "invalid requests" - {
    "when it isn't the player's turn" - {
      "cannot fold" ignore {
        TODO
      }
      "cannot bet" ignore {
        TODO
      }
      "cannot check" ignore {
        TODO
      }
    }

    "bet cannot exceed stack" ignore {
      TODO
    }
  }

  private def gameFixture(
    contextBuilder: PlayerAddress => AppContext[IO],
    initialSeed: Long,
    startingStack: Option[Int],
    initialSmallBlind: Option[Int],
    timerConfig: Option[List[TimerLevel]],
  )(implicit pos: Position): IO[(GameStatus, Welcome, Welcome, Welcome, Welcome)] = {
    for {
      hostResponse <- performCreateGame(createGameRequest, contextBuilder(hostAddress), initialSeed)
      hostWelcome = hostResponse.messages.find { case (address, _) =>
        address == hostAddress
      }.map(_._2).value
      gameCode = hostWelcome.gameCode
      p1JoinResponse <- performJoinGame(joinGameRequest(gameCode, "player-1"), contextBuilder(player1Address))
      p1Welcome = p1JoinResponse.messages.get(player1Address).value
      p2JoinResponse <- performJoinGame(joinGameRequest(gameCode, "player-2"), contextBuilder(player2Address))
      p2Welcome = p2JoinResponse.messages.get(player2Address).value
      p3JoinResponse <- performJoinGame(joinGameRequest(gameCode, "player-3"), contextBuilder(player3Address))
      p3Welcome = p3JoinResponse.messages.get(player3Address).value
      startRequest = startGameRequest(hostWelcome, startingStack, initialSmallBlind, timerConfig,
        List(hostWelcome.playerId, p1Welcome.playerId, p2Welcome.playerId, p3Welcome.playerId)
      )
      startResponse <- performStartGame(startRequest, contextBuilder(hostAddress))
      gameStatus = startResponse.statuses.get(hostAddress).value
    } yield (gameStatus, hostWelcome, p1Welcome, p2Welcome, p3Welcome)
  }
}
