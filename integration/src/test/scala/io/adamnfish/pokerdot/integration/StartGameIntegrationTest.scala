package io.adamnfish.pokerdot.integration

import io.adamnfish.pokerdot.TestHelpers.parseReq
import io.adamnfish.pokerdot.integration.CreateGameIntegrationTest.{createGameRequest, performCreateGame}
import io.adamnfish.pokerdot.integration.JoinGameIntegrationTest.{joinGameRequest, performJoinGame}
import io.adamnfish.pokerdot.integration.StartGameIntegrationTest.{performStartGame, startGameRequest}
import io.adamnfish.pokerdot.logic.Utils
import io.adamnfish.pokerdot.{PokerDot, TestDates, TestHelpers}
import io.adamnfish.pokerdot.models.Serialisation.RequestEncoders.startGameEncoder
import io.adamnfish.pokerdot.models.{AppContext, Attempt, BreakLevel, GameId, GameStartedSummary, GameStatus, PlayerAddress, PlayerId, PlayerKey, PreFlop, PreFlopSummary, Response, RoundLevel, StartGame, TimerLevel, TimerStatus, Welcome}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import io.circe.syntax._
import org.scalactic.source.Position
import org.scalatest.OptionValues


class StartGameIntegrationTest extends AnyFreeSpec with Matchers with IntegrationComponents with TestHelpers with OptionValues {
  val initialSeed = 1L
  val hostAddress = PlayerAddress("host-address")
  val player1Address = PlayerAddress("player-1-address")
  val player2Address = PlayerAddress("player-2-address")

  "for a basic start game call" - {
    "is successful" in withAppContext { (context, _) =>
      val (hostWelcome, p1Welcome, p2Welcome) = gameFixture(context).value()
      val playerOrder = List(hostWelcome.playerId, p1Welcome.playerId, p2Welcome.playerId)
      performStartGame(startGameRequest(hostWelcome, None, None, None, playerOrder), context(hostAddress)) is ASuccess
    }

    "sends status messages" - {
      "to every player" in withAppContext { (context, _) =>
        val (hostWelcome, p1Welcome, p2Welcome) = gameFixture(context).value()
        val playerOrder = List(hostWelcome.playerId, p1Welcome.playerId, p2Welcome.playerId)
        val response = performStartGame(startGameRequest(hostWelcome, None, None, None, playerOrder), context(hostAddress)).value()
        response.statuses.keys should(contain.allOf(hostAddress, player1Address, player2Address))
      }

      "with the game started action" in withAppContext { (context, _) =>
        val (hostWelcome, p1Welcome, p2Welcome) = gameFixture(context).value()
        val playerOrder = List(hostWelcome.playerId, p1Welcome.playerId, p2Welcome.playerId)
        val response = performStartGame(startGameRequest(hostWelcome, None, None, None, playerOrder), context(hostAddress)).value()
        response.statuses.values.toList.map(_.action).distinct shouldEqual List(GameStartedSummary())
      }
    }

    "the player order" - {
      "is reflected in the status message's game" in withAppContext { (context, _) =>
        val (hostWelcome, p1Welcome, p2Welcome) = gameFixture(context).value()
        val playerOrder = List(
          p2Welcome.playerId, p1Welcome.playerId, hostWelcome.playerId
        )
        val response = performStartGame(startGameRequest(hostWelcome, None, None, None, playerOrder), context(hostAddress)).value()
        val gameStatusMessage = response.statuses.get(hostAddress).value
        gameStatusMessage.game.players.map(_.playerId) shouldEqual playerOrder
      }

      "is persisted to the database" in withAppContext { (context, db) =>
        val (hostWelcome, p1Welcome, p2Welcome) = gameFixture(context).value()
        val playerOrder = List(
          p2Welcome.playerId, p1Welcome.playerId, hostWelcome.playerId
        )
        performStartGame(startGameRequest(hostWelcome, None, None, None, playerOrder), context(hostAddress)).value()
        val gameDb = db.getGame(hostWelcome.gameId).value().value
        gameDb.playerIds shouldEqual playerOrder.map(_.pid)
      }

      "determines the initial `inTurn` player - player after dealer and blinds" in withAppContext { (context, _) =>
        val (hostWelcome, p1Welcome, p2Welcome) = gameFixture(context).value()
        val playerOrder = List(
          p2Welcome.playerId, p1Welcome.playerId, hostWelcome.playerId
        )
        val response = performStartGame(startGameRequest(hostWelcome, None, None, None, playerOrder), context(hostAddress)).value()
        val gameStatusMessage = response.statuses.get(hostAddress).value
        gameStatusMessage.game.inTurn shouldEqual Some(p2Welcome.playerId)
      }
    }

    "persists the game to the database" in withAppContext { (context, db) =>
      val (hostWelcome, p1Welcome, p2Welcome) = gameFixture(context).value()
      val playerOrder = List(
        p2Welcome.playerId, p1Welcome.playerId, hostWelcome.playerId
      )
      performStartGame(startGameRequest(hostWelcome, None, None, None, playerOrder), context(hostAddress)).value()
      val gameDb = db.getGame(hostWelcome.gameId).value().value
      gameDb should have(
        "started" as true,
        "startTime" as TestDates.now(),
        "expiry" as TestDates.expires(),
        "button" as 0,
      )
    }

    "persists the players to the database" in withAppContext { (context, db) =>
      val (hostWelcome, p1Welcome, p2Welcome) = gameFixture(context).value()
      val playerOrder = List(
        p2Welcome.playerId, p1Welcome.playerId, hostWelcome.playerId
      )
      performStartGame(startGameRequest(hostWelcome, None, None, None, playerOrder), context(hostAddress)).value()
      val playerDbs = db.getPlayers(hostWelcome.gameId).value()
      playerDbs.map(_.playerId).toSet shouldEqual playerOrder.map(_.pid).toSet
    }
  }

  "if starting stack is provided" - {
    val initialStack = 1000

    "if initial small blind is provided" - {
      val initialSmallBlind = 5

      "sends game status messages with the correct game state" in withAppContext { (context, _) =>
        val (hostWelcome, p1Welcome, p2Welcome) = gameFixture(context).value()
        val playerOrder = List(hostWelcome.playerId, p1Welcome.playerId, p2Welcome.playerId)
        val response = performStartGame(startGameRequest(hostWelcome, Some(initialStack), Some(initialSmallBlind), None, playerOrder), context(hostAddress)).value()
        val gameStatus = response.statuses.get(hostAddress).value
        gameStatus.game should have(
          "round" as PreFlopSummary(),
          "smallBlind" as initialSmallBlind,
          // ...
        )
      }

      "persists the correct game state" in withAppContext { (context, db) =>
        val (hostWelcome, p1Welcome, p2Welcome) = gameFixture(context).value()
        val playerOrder = List(hostWelcome.playerId, p1Welcome.playerId, p2Welcome.playerId)
        performStartGame(startGameRequest(hostWelcome, Some(initialStack), Some(initialSmallBlind), None, playerOrder), context(hostAddress)).value()
        val gameDb = db.getGame(hostWelcome.gameId).value().value
        gameDb should have(
          "smallBlind" as initialSmallBlind,
          "timer" as None,
          "trackStacks" as true,
          "phase" as PreFlop,
        )
      }

      "saves the initial stack config to each player" in withAppContext { (context, db) =>
        val (hostWelcome, p1Welcome, p2Welcome) = gameFixture(context).value()
        val playerOrder = List(hostWelcome.playerId, p1Welcome.playerId, p2Welcome.playerId)
        performStartGame(startGameRequest(hostWelcome, Some(initialStack), Some(initialSmallBlind), None, playerOrder), context(hostAddress)).value()
        val playerDbs = db.getPlayers(hostWelcome.gameId).value()
        playerDbs.map(pdb => pdb.stack + pdb.bet).distinct shouldEqual List(initialStack)
      }
    }

    "if a timer config is provided" - {
      val timerConfig = List(
        RoundLevel(300, 5),
        RoundLevel(300, 10),
        BreakLevel(150),
        RoundLevel(450, 20),
        RoundLevel(450, 50),
      )

      "sends game status messages with the correct game state" in withAppContext { (context, _) =>
        val (hostWelcome, p1Welcome, p2Welcome) = gameFixture(context).value()
        val playerOrder = List(hostWelcome.playerId, p1Welcome.playerId, p2Welcome.playerId)
        val response = performStartGame(startGameRequest(hostWelcome, Some(initialStack), None, Some(timerConfig), playerOrder), context(hostAddress)).value()
        val gameStatus = response.statuses.get(hostAddress).value
        gameStatus.game should have(
          "round" as PreFlopSummary(),
          "smallBlind" as 5,
          // ...
        )
      }

      "persists the game information, including the time config"  in withAppContext { (context, db) =>
        val (hostWelcome, p1Welcome, p2Welcome) = gameFixture(context).value()
        val playerOrder = List(hostWelcome.playerId, p1Welcome.playerId, p2Welcome.playerId)
        performStartGame(startGameRequest(hostWelcome, Some(initialStack), None, Some(timerConfig), playerOrder), context(hostAddress)).value()
        val gameDb = db.getGame(hostWelcome.gameId).value().value
        gameDb should have(
          "smallBlind" as 5,
          "trackStacks" as true,
          "phase" as PreFlop,
          "timer" as Some(
            TimerStatus(
              0L, None, timerConfig
            )
          ),
        )
      }

      "saves the initial stack config to each player" in withAppContext { (context, db) =>
        val (hostWelcome, p1Welcome, p2Welcome) = gameFixture(context).value()
        val playerOrder = List(hostWelcome.playerId, p1Welcome.playerId, p2Welcome.playerId)
        performStartGame(startGameRequest(hostWelcome, Some(initialStack), None, Some(timerConfig), playerOrder), context(hostAddress)).value()
        val playerDbs = db.getPlayers(hostWelcome.gameId).value()
        playerDbs.map(pdb => pdb.stack + pdb.bet).distinct shouldEqual List(initialStack)
      }

      "player blind payments should be persisted" in withAppContext { (context, db) =>
        val (hostWelcome, p1Welcome, p2Welcome) = gameFixture(context).value()
        val playerOrder = List(hostWelcome.playerId, p1Welcome.playerId, p2Welcome.playerId)
        performStartGame(startGameRequest(hostWelcome, Some(initialStack), None, Some(timerConfig), playerOrder), context(hostAddress)).value()
        val playerDbs = db.getPlayers(hostWelcome.gameId).value()
        // order playerdb results to match game order
        playerDbs.sortBy(pdb => playerOrder.map(_.pid).indexOf(pdb.playerId))
          .map(pdb => pdb.bet) shouldEqual List(0, 5, 10)
      }
    }

    "fails if neither timer config nor initial small blind are provided"  in withAppContext { (context, _) =>
      val (hostWelcome, p1Welcome, p2Welcome) = gameFixture(context).value()
      val playerOrder = List(hostWelcome.playerId, p1Welcome.playerId, p2Welcome.playerId)
      val result = performStartGame(startGameRequest(hostWelcome, Some(initialStack), None, None, playerOrder), context(hostAddress))
      result is AFailure
    }
  }

  "fails, when" - {
    "the game has already started" in withAppContext { (context, _) =>
      val (hostWelcome, p1Welcome, p2Welcome) = gameFixture(context).value()
      val playerOrder = List(hostWelcome.playerId, p1Welcome.playerId, p2Welcome.playerId)
      performStartGame(startGameRequest(hostWelcome, None, None, None, playerOrder), context(hostAddress)).value()
      // start game a second time
      val result = performStartGame(startGameRequest(hostWelcome, None, None, None, playerOrder), context(hostAddress))
      result is AFailure
    }

    "the request is not a valid start game request" in withAppContext { (context, _) =>
      val (hostWelcome, p1Welcome, p2Welcome) = gameFixture(context).value()
      val result = performStartGame("""{"foo":"bar"}""", context(hostAddress))
      result is AFailure
    }

    "the player making the call is not the host" in withAppContext { (context, _) =>
      val (hostWelcome, p1Welcome, p2Welcome) = gameFixture(context).value()
      val playerOrder = List(hostWelcome.playerId, p1Welcome.playerId, p2Welcome.playerId)
      val result = performStartGame(startGameRequest(p1Welcome, None, None, None, playerOrder), context(player1Address))
      result is AFailure
    }

    "this player has not joined this game" in withAppContext { (context, _) =>
      val (hostWelcome, p1Welcome, p2Welcome) = gameFixture(context).value()
      val playerOrder = List(hostWelcome.playerId, p1Welcome.playerId, p2Welcome.playerId)
      val playerAddress = PlayerAddress("another-address")
      val request = StartGame(
        hostWelcome.gameId,
        PlayerId("different-id"),
        PlayerKey("different-key"),
        None, None, None,
        playerOrder,
      ).asJson.noSpaces
      val result = performStartGame(request, context(playerAddress))
      result is AFailure
    }
  }

  private def gameFixture(contextBuilder: PlayerAddress => AppContext)(implicit pos: Position): Attempt[(Welcome, Welcome, Welcome)] = {
    for {
      hostResponse <- performCreateGame(createGameRequest, contextBuilder(hostAddress), initialSeed)
      hostWelcome = hostResponse.messages.find { case (address, _) =>
        address == hostAddress
      }.map(_._2).value
      gameCode = hostWelcome.gameCode
      p1JoinResponse <- performJoinGame(joinGameRequest(gameCode, "player-1"), contextBuilder(player1Address))
      p1Welcome = p1JoinResponse.messages.find { case (address, _) =>
        address == player1Address
      }.map(_._2).value
      p2JoinResponse <- performJoinGame(joinGameRequest(gameCode, "player-2"), contextBuilder(player2Address))
      p2Welcome = p2JoinResponse.messages.find { case (address, _) =>
        address == player2Address
      }.map(_._2).value
    } yield (hostWelcome, p1Welcome, p2Welcome)
  }
}

object StartGameIntegrationTest {
  def startGameRequest(
    welcome: Welcome,
    startingStack: Option[Int],
    initialSmallBlind: Option[Int],
    timerConfig: Option[List[TimerLevel]],
    playerOrder: List[PlayerId],
  ): String = {
    val request = StartGame(
      welcome.gameId,
      welcome.playerId,
      welcome.playerKey,
      startingStack,
      initialSmallBlind,
      timerConfig,
      playerOrder,
    )
    request.asJson.noSpaces
  }

  def performStartGame(request: String, appContext: AppContext): Attempt[Response[GameStatus]] = {
    PokerDot.startGame(parseReq(request), appContext)
  }
}