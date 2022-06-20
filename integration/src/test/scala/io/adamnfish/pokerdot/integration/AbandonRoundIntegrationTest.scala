package io.adamnfish.pokerdot.integration

import io.adamnfish.pokerdot.{PokerDot, TestHelpers}
import io.adamnfish.pokerdot.integration.CreateGameIntegrationTest.{createGameRequest, performCreateGame}
import io.adamnfish.pokerdot.integration.IntegrationComponents.{abandonRoundRequest, betRequest, checkRequest, foldRequest}
import io.adamnfish.pokerdot.integration.JoinGameIntegrationTest.{joinGameRequest, performJoinGame}
import io.adamnfish.pokerdot.integration.StartGameIntegrationTest.{performStartGame, startGameRequest}
import io.adamnfish.pokerdot.models.{AppContext, Attempt, GameStatus, PlayerAddress, PlayerId, PreFlop, TimerLevel, Welcome}
import org.scalactic.source.Position
import org.scalatest.OptionValues
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers


class AbandonRoundIntegrationTest extends AnyFreeSpec with Matchers with IntegrationComponents with TestHelpers with OptionValues {
  val hostAddress = PlayerAddress("host-address")
  val player1Address = PlayerAddress("player-1-address")
  val player2Address = PlayerAddress("player-2-address")

  "can reset the first round of a game"  in withAppContext { (context, db) =>
    val (_, hostWelcome, p1Welcome, p2Welcome) = gameFixture(context,
      initialSeed = 10L, // determines deck order
      startingStack = Some(1000),
      initialSmallBlind = Some(5),
      timerConfig = None,
    ).value()

    // perform some actions
    PokerDot.pokerdot(foldRequest(hostWelcome), context(hostAddress)).value()
    PokerDot.pokerdot(betRequest(5, p1Welcome), context(player1Address)).value()
    PokerDot.pokerdot(checkRequest(p2Welcome), context(player2Address)).value()

    val preAbandonGameDb = db.getGame(hostWelcome.gameId).value().value

    // host issues an abandon round request
    PokerDot.pokerdot(abandonRoundRequest(hostWelcome), context(hostAddress)).value()

    val postAbandonGameDb = db.getGame(hostWelcome.gameId).value().value
    // changing the seed causes new cards to be dealt
    postAbandonGameDb.seed should not equal preAbandonGameDb.seed
    // the dealer does not move
    postAbandonGameDb.button shouldEqual preAbandonGameDb.button
    // phase is reset to the start of the round
    postAbandonGameDb.phase shouldEqual PreFlop

    val playerDbsShowdown = db.getPlayers(hostWelcome.gameId).value().map(pdb => (PlayerId(pdb.playerId), pdb)).toMap
    playerDbsShowdown.get(hostWelcome.playerId).value should have(
      "stack" as 1000,
      "bet" as 0,
      "pot" as 0,
      "checked" as false,
      "folded" as false,
      "blind" as 0,
    )
    playerDbsShowdown.get(p1Welcome.playerId).value should have(
      "stack" as 1000,
      "bet" as 0,
      "pot" as 0,
      "checked" as false,
      "folded" as false,
      "blind" as 1,
    )
    playerDbsShowdown.get(p2Welcome.playerId).value should have(
      "stack" as 1000,
      "bet" as 0,
      "pot" as 0,
      "checked" as false,
      "folded" as false,
      "blind" as 2,
    )
  }

  "can reset a later round" ignore {}

  "a round can be abandoned even if players haven't acted"  in withAppContext { (context, db) =>
    // mostly this makes sure the -ve integration tests below are working
    val (_, hostWelcome, p1Welcome, p2Welcome) = gameFixture(context,
      initialSeed = 10L, // determines deck order
      startingStack = Some(1000),
      initialSmallBlind = Some(5),
      timerConfig = None,
    ).value()
    val preAbandonGameDb = db.getGame(hostWelcome.gameId).value().value

    PokerDot.pokerdot(abandonRoundRequest(hostWelcome), context(hostAddress)).value()

    val postAbandonGameDb = db.getGame(hostWelcome.gameId).value().value
    // changing the seed causes new cards to be dealt
    postAbandonGameDb.seed should not equal preAbandonGameDb.seed
  }

  "invalid requests" - {
    "an otherwise valid request fails if the player is not an admin" in withAppContext { (context, _) =>
      val (_, hostWelcome, p1Welcome, p2Welcome) = gameFixture(context,
        initialSeed = 10L, // determines deck order
        startingStack = Some(1000),
        initialSmallBlind = Some(5),
        timerConfig = None,
      ).value()

      PokerDot.pokerdot(abandonRoundRequest(p1Welcome), context(player1Address)) is AFailure
    }

    "fails if the game has not started" ignore {}
    "fails if the player isn't in this game" ignore {}
    "fails if the player key is not correct" ignore {}

    // TODO: should we deny abandon round requests after we are in a showdown?
  }

  private def gameFixture(
    contextBuilder: PlayerAddress => AppContext,
    initialSeed: Long,
    startingStack: Option[Int],
    initialSmallBlind: Option[Int],
    timerConfig: Option[List[TimerLevel]],
  )(implicit pos: Position): Attempt[(GameStatus, Welcome, Welcome, Welcome)] = {
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
      startRequest = startGameRequest(hostWelcome, startingStack, initialSmallBlind, timerConfig,
        List(hostWelcome.playerId, p1Welcome.playerId, p2Welcome.playerId)
      )
      startResponse <- performStartGame(startRequest, contextBuilder(hostAddress))
      gameStatus = startResponse.statuses.get(hostAddress).value
    } yield (gameStatus, hostWelcome, p1Welcome, p2Welcome)
  }

}
