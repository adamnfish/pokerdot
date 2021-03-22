package io.adamnfish.pokerdot.integration

import io.adamnfish.pokerdot.TestHelpers.parseReq
import io.adamnfish.pokerdot.integration.CreateGameIntegrationTest.{createGameRequest, performCreateGame}
import io.adamnfish.pokerdot.integration.GameplayIntegrationTest.{advancePhaseRequest, betRequest, checkRequest, foldRequest}
import io.adamnfish.pokerdot.integration.JoinGameIntegrationTest.{joinGameRequest, performJoinGame}
import io.adamnfish.pokerdot.integration.StartGameIntegrationTest.{performStartGame, startGameRequest}
import io.adamnfish.pokerdot.logic.Cards.RichRank
import io.adamnfish.pokerdot.models.Serialisation.RequestEncoders.encodeRequest
import io.adamnfish.pokerdot.models._
import io.adamnfish.pokerdot.{PokerDot, TestHelpers}
import org.scalactic.source.Position
import org.scalatest.OptionValues
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers


class GameplayIntegrationTest extends AnyFreeSpec with Matchers with IntegrationComponents with TestHelpers with OptionValues {
  val hostAddress = PlayerAddress("host-address")
  val player1Address = PlayerAddress("player-1-address")
  val player2Address = PlayerAddress("player-2-address")
  val player3Address = PlayerAddress("player-3-address")

  "poker gameplay works" - {
    "for an example game" in withAppContext { (context, db) =>
      val (_, hostWelcome, p1Welcome, p2Welcome, p3Welcome) = gameFixture(context,
        initialSeed = 0L, // determines deck order
        startingStack = Some(1000),
        initialSmallBlind = Some(5),
        timerConfig = None,
      ).value()
      // community: K♦  A♦  Q♠  6♥  J♣
      //   host:    Q♦  7♣
      //   p1:      10♠ 7♦
      //   p2:      Q♣  J♥
      //   p3:      7♠  6♠
      // host is dealer, p1 small blind, p2 big blind, p3 first to act
      // p3 is initial player (left of dealer small blind and big blind)
      // p3 gas 7♠ 6♠ and folds
      PokerDot.pokerdot(foldRequest(p3Welcome), context(player3Address)).value()
      // host has Q♦ 7♣ and calls
      PokerDot.pokerdot(betRequest(10, hostWelcome), context(hostAddress)).value()
      // p1 has 10♠ 7♦ and calls from small blind
      PokerDot.pokerdot(betRequest(5, p1Welcome), context(player1Address)).value()
      // p2 has J♥ Q♣ and checks from big blind
      PokerDot.pokerdot(checkRequest(p2Welcome), context(player2Address)).value()
      // phase is now complete
      // TODO: check database for player states here
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
      PokerDot.pokerdot(advancePhaseRequest(hostWelcome), context(hostAddress)).value()

      // community cards K♦ A♦ Q♠ are now visible
      // p1 is first to act, and checks
      PokerDot.pokerdot(checkRequest(p1Welcome), context(player1Address)).value()
      // p2 bets
      PokerDot.pokerdot(betRequest(10, p2Welcome), context(player2Address)).value()
      // p3 has folded
      // host calls
      PokerDot.pokerdot(betRequest(10, hostWelcome), context(hostAddress)).value()
      // p1 needs to react to the bet, and calls with a straight draw
      PokerDot.pokerdot(betRequest(10, p1Welcome), context(player1Address)).value()
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
      PokerDot.pokerdot(advancePhaseRequest(hostWelcome), context(hostAddress)).value()

      // community cards K♦ A♦ Q♠ 6♥ are now visible
      // players are cautious about overcards and all check
      PokerDot.pokerdot(checkRequest(p1Welcome), context(player1Address)).value()
      PokerDot.pokerdot(checkRequest(p2Welcome), context(player2Address)).value()
      PokerDot.pokerdot(checkRequest(hostWelcome), context(hostAddress)).value()
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
      PokerDot.pokerdot(advancePhaseRequest(hostWelcome), context(hostAddress)).value()

      // all community cards now visible K♦ A♦ Q♠ 6♥ J♣
      // p1 has lucked a straight, and bets
      PokerDot.pokerdot(betRequest(50, p1Welcome), context(player1Address)).value()
      // p2 has two-pair, decides to call
      PokerDot.pokerdot(betRequest(50, p2Welcome), context(player2Address)).value()
      // host only has pair of queens and will let these two fight it out
      PokerDot.pokerdot(foldRequest(hostWelcome), context(hostAddress)).value()
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

      val response = PokerDot.advancePhase(parseReq(advancePhaseRequest(hostWelcome)), context(hostAddress)).value()

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
        PlayerWinnings(hostWelcome.playerId, Pair(Queen of Spades, Queen of Diamonds, Ace of Diamonds, King of Diamonds, Jack of Clubs), 0),
        PlayerWinnings(p1Welcome.playerId, Straight(Ace of Diamonds, King of Diamonds, Queen of Spades, Jack of Clubs, Ten of Spades), 160),
        PlayerWinnings(p2Welcome.playerId, TwoPair(Queen of Spades, Queen of Clubs, Jack of Hearts, Jack of Clubs, Ace of Diamonds), 0),
        PlayerWinnings(p3Welcome.playerId, Pair(Six of Spades, Six of Hearts, Ace of Diamonds, King of Diamonds, Queen of Spades), 0),
      )
      // a single pot between players 1 and 2, with player 1 winning
      roundWinnings.pots shouldEqual List(
        PotWinnings(160, Set(p1Welcome.playerId, p2Welcome.playerId), Set(p1Welcome.playerId))
      )

      // advance to next round
      PokerDot.advancePhase(parseReq(advancePhaseRequest(hostWelcome)), context(hostAddress)).value()

      // players should be reset for the new round
      val playerDbsPreFlop2 = db.getPlayers(hostWelcome.gameId).value().map(pdb => (PlayerId(pdb.playerId), pdb)).toMap
      playerDbsPreFlop2.get(hostWelcome.playerId).value should have(
        "stack" as 980,
        "checked" as false,
        "folded" as false,
        "bet" as 0,
        "pot" as 0,
      )
      playerDbsPreFlop2.get(p1Welcome.playerId).value should have(
        "stack" as 1090,
        "checked" as false,
        "folded" as false,
        "bet" as 0,
        "pot" as 0,
      )
      playerDbsPreFlop2.get(p2Welcome.playerId).value should have(
        "stack" as 925, // small blind paid out as well as prev round's result
        "checked" as false,
        "folded" as false,
        "bet" as 5,
        "pot" as 0,
        "blind" as 1,
      )
      playerDbsPreFlop2.get(p3Welcome.playerId).value should have(
        "stack" as 990, // big blind paid out as well as prev round's result
        "checked" as false,
        "folded" as false,
        "bet" as 10,
        "pot" as 0,
        "blind" as 2,
      )
      // dealer should have moved correctly
      db.getGame(hostWelcome.gameId).value().value should have(
        "button" as 1,
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
  )(implicit pos: Position): Attempt[(GameStatus, Welcome, Welcome, Welcome, Welcome)] = {
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
object GameplayIntegrationTest {
  def betRequest(betAmount: Int, welcome: Welcome): String = {
    val request = Bet(welcome.gameId, welcome.playerKey, welcome.playerId, betAmount)
    encodeRequest(request).noSpaces
  }

  def checkRequest(welcome: Welcome): String = {
    val request = Check(welcome.gameId, welcome.playerKey, welcome.playerId)
    encodeRequest(request).noSpaces
  }

  def foldRequest(welcome: Welcome): String = {
    val request = Fold(welcome.gameId, welcome.playerKey, welcome.playerId)
    encodeRequest(request).noSpaces
  }

  def advancePhaseRequest(welcome: Welcome): String = {
    val request = AdvancePhase(welcome.gameId, welcome.playerKey, welcome.playerId)
    encodeRequest(request).noSpaces
  }
}