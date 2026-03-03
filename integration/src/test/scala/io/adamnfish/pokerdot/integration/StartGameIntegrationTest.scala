package io.adamnfish.pokerdot.integration

import io.adamnfish.pokerdot.TestHelpers.parseReq
import io.adamnfish.pokerdot.integration.CreateGameIntegrationTest.{
  createGameRequest,
  performCreateGame
}
import io.adamnfish.pokerdot.integration.JoinGameIntegrationTest.{
  joinGameRequest,
  performJoinGame
}
import io.adamnfish.pokerdot.integration.StartGameIntegrationTest.{
  performStartGame,
  startGameRequest
}
import io.adamnfish.pokerdot.logic.Games
import io.adamnfish.pokerdot.models.Serialisation.RequestEncoders.encodeRequest
import io.adamnfish.pokerdot.models.*
import io.adamnfish.pokerdot.{PokerDot, TestHelpers, TestTime}
import io.circe.syntax.*
import cats.effect.*
import cats.effect.testing.scalatest.AsyncIOSpec
import org.scalactic.source.Position
import org.scalatest.OptionValues
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers

class StartGameIntegrationTest
    extends AsyncFreeSpec
    with AsyncIOSpec
    with Matchers
    with IntegrationComponents
    with TestHelpers
    with OptionValues {
  val initialSeed = 1L
  val hostAddress = PlayerAddress("host-address")
  val player1Address = PlayerAddress("player-1-address")
  val player2Address = PlayerAddress("player-2-address")

  "for a basic start game call" - {
    "is successful" in appContextRes.use { (context, _) =>
      for {
        (hostWelcome, p1Welcome, p2Welcome) <- gameFixture(context)
        playerOrder = List(
          hostWelcome.playerId,
          p1Welcome.playerId,
          p2Welcome.playerId
        )
        _ <- performStartGame(
          startGameRequest(hostWelcome, None, None, None, playerOrder),
          context(hostAddress)
        )
      } yield assert(true)
    }

    "sends status messages" - {
      "to every player" in appContextRes.use { (context, _) =>
        for {
          (hostWelcome, p1Welcome, p2Welcome) <- gameFixture(context)
          playerOrder = List(
            hostWelcome.playerId,
            p1Welcome.playerId,
            p2Welcome.playerId
          )
          response <- performStartGame(
            startGameRequest(hostWelcome, None, None, None, playerOrder),
            context(hostAddress)
          )
        } yield response.statuses.keys should (contain.allOf(
          hostAddress,
          player1Address,
          player2Address
        ))
      }

      "with the game started action" in appContextRes.use { (context, _) =>
        for {
          (hostWelcome, p1Welcome, p2Welcome) <- gameFixture(context)
          playerOrder = List(
            hostWelcome.playerId,
            p1Welcome.playerId,
            p2Welcome.playerId
          )
          response <- performStartGame(
            startGameRequest(hostWelcome, None, None, None, playerOrder),
            context(hostAddress)
          )
        } yield response.statuses.values.toList
          .map(_.action)
          .distinct shouldEqual List(GameStartedSummary())
      }
    }

    "the player order" - {
      "is reflected in the status message's game" in appContextRes.use {
        (context, _) =>
          for {
            (hostWelcome, p1Welcome, p2Welcome) <- gameFixture(context)
            playerOrder = List(
              p2Welcome.playerId,
              p1Welcome.playerId,
              hostWelcome.playerId
            )
            response <- performStartGame(
              startGameRequest(hostWelcome, None, None, None, playerOrder),
              context(hostAddress)
            )
            gameStatusMessage = response.statuses.get(hostAddress).value
          } yield {
            gameStatusMessage.game.players.map(
              _.playerId
            ) shouldEqual playerOrder
          }
      }

      "is persisted to the database" in appContextRes.use { (context, db) =>
        for {
          (hostWelcome, p1Welcome, p2Welcome) <- gameFixture(context)
          playerOrder = List(
            p2Welcome.playerId,
            p1Welcome.playerId,
            hostWelcome.playerId
          )
          _ <- performStartGame(
            startGameRequest(hostWelcome, None, None, None, playerOrder),
            context(hostAddress)
          )
          gameDbOpt <- db.getGame(hostWelcome.gameId)
        } yield gameDbOpt.value.playerIds shouldEqual playerOrder.map(_.pid)
      }

      "determines the initial `inTurn` player - player after dealer and blinds" in appContextRes.use {
        (context, _) =>
          for {
            (hostWelcome, p1Welcome, p2Welcome) <- gameFixture(context)
            playerOrder = List(
              p2Welcome.playerId,
              p1Welcome.playerId,
              hostWelcome.playerId
            )
            response <- performStartGame(
              startGameRequest(hostWelcome, None, None, None, playerOrder),
              context(hostAddress)
            )
            gameStatusMessage = response.statuses.get(hostAddress).value
          } yield gameStatusMessage.game.inTurn shouldEqual Some(
            p2Welcome.playerId
          )
      }
    }

    "persists the game to the database" in appContextRes.use { (context, db) =>
      for {
        (hostWelcome, p1Welcome, p2Welcome) <- gameFixture(context)
        playerOrder = List(
          p2Welcome.playerId,
          p1Welcome.playerId,
          hostWelcome.playerId
        )
        _ <- performStartGame(
          startGameRequest(hostWelcome, None, None, None, playerOrder),
          context(hostAddress)
        )
        gameDbOpt <- db.getGame(hostWelcome.gameId)
        now <- context(hostAddress).time.now
      } yield gameDbOpt.value should have(
        "started" as true,
        "startTime" as now,
        "expiry" as Games.expiryTime(now),
        "button" as 0
      )
    }

    "persists the players to the database" in appContextRes.use { (context, db) =>
      for {
        (hostWelcome, p1Welcome, p2Welcome) <- gameFixture(context)
        playerOrder = List(
          p2Welcome.playerId,
          p1Welcome.playerId,
          hostWelcome.playerId
        )
        _ <- performStartGame(
          startGameRequest(hostWelcome, None, None, None, playerOrder),
          context(hostAddress)
        )
        playerDbs <- db.getPlayers(hostWelcome.gameId)
      } yield playerDbs.map(_.playerId).toSet shouldEqual playerOrder
        .map(_.pid)
        .toSet
    }
  }

  "if starting stack is provided" - {
    val initialStack = 1000

    "if initial small blind is provided" - {
      val initialSmallBlind = 5

      "sends game status messages with the correct game state" in appContextRes.use {
        (context, _) =>
          for {
            (hostWelcome, p1Welcome, p2Welcome) <- gameFixture(context)
            playerOrder = List(
              hostWelcome.playerId,
              p1Welcome.playerId,
              p2Welcome.playerId
            )
            response <- performStartGame(
              startGameRequest(
                hostWelcome,
                Some(initialStack),
                Some(initialSmallBlind),
                None,
                playerOrder
              ),
              context(hostAddress)
            )
            gameStatus = response.statuses.get(hostAddress).value
          } yield gameStatus.game should have(
            "round" as PreFlopSummary(),
            "smallBlind" as initialSmallBlind
            // ...
          )
      }

      "persists the correct game state" in appContextRes.use { (context, db) =>
        for {
          (hostWelcome, p1Welcome, p2Welcome) <- gameFixture(context)
          playerOrder = List(
            hostWelcome.playerId,
            p1Welcome.playerId,
            p2Welcome.playerId
          )
          _ <- performStartGame(
            startGameRequest(
              hostWelcome,
              Some(initialStack),
              Some(initialSmallBlind),
              None,
              playerOrder
            ),
            context(hostAddress)
          )
          gameDbOpt <- db.getGame(hostWelcome.gameId)
        } yield gameDbOpt.value should have(
          "smallBlind" as initialSmallBlind,
          "timer" as None,
          "trackStacks" as true,
          "phase" as PreFlop
        )
      }

      "saves the initial stack config to each player" in appContextRes.use {
        (context, db) =>
          for {
            (hostWelcome, p1Welcome, p2Welcome) <- gameFixture(context)
            playerOrder = List(
              hostWelcome.playerId,
              p1Welcome.playerId,
              p2Welcome.playerId
            )
            _ <- performStartGame(
              startGameRequest(
                hostWelcome,
                Some(initialStack),
                Some(initialSmallBlind),
                None,
                playerOrder
              ),
              context(hostAddress)
            )
            playerDbs <- db.getPlayers(hostWelcome.gameId)
          } yield playerDbs
            .map(pdb => pdb.stack + pdb.bet)
            .distinct shouldEqual List(
            initialStack
          )
      }
    }

    "if a timer config is provided" - {
      val timerConfig = List(
        RoundLevel(300, 5),
        RoundLevel(300, 10),
        BreakLevel(150),
        RoundLevel(450, 20),
        RoundLevel(450, 50)
      )

      "sends game status messages with the correct game state" in appContextRes.use {
        (context, _) =>
          for {
            (hostWelcome, p1Welcome, p2Welcome) <- gameFixture(context)
            playerOrder = List(
              hostWelcome.playerId,
              p1Welcome.playerId,
              p2Welcome.playerId
            )
            response <- performStartGame(
              startGameRequest(
                hostWelcome,
                Some(initialStack),
                None,
                Some(timerConfig),
                playerOrder
              ),
              context(hostAddress)
            )
            gameStatus = response.statuses.get(hostAddress).value
          } yield gameStatus.game should have(
            "round" as PreFlopSummary(),
            "smallBlind" as 5
            // ...
          )
      }

      "persists the game information, including the time config" in appContextRes.use {
        (context, db) =>
          for {
            (hostWelcome, p1Welcome, p2Welcome) <- gameFixture(context)
            playerOrder = List(
              hostWelcome.playerId,
              p1Welcome.playerId,
              p2Welcome.playerId
            )
            _ <- performStartGame(
              startGameRequest(
                hostWelcome,
                Some(initialStack),
                None,
                Some(timerConfig),
                playerOrder
              ),
              context(hostAddress)
            )
            gameDbOpt <- db.getGame(hostWelcome.gameId)
          } yield gameDbOpt.value should have(
            "smallBlind" as 5,
            "trackStacks" as true,
            "phase" as PreFlop,
            "timer" as Some(
              TimerStatus(
                0L,
                None,
                timerConfig
              )
            )
          )
      }

      "saves the initial stack config to each player" in appContextRes.use {
        (context, db) =>
          for {
            (hostWelcome, p1Welcome, p2Welcome) <- gameFixture(context)
            playerOrder = List(
              hostWelcome.playerId,
              p1Welcome.playerId,
              p2Welcome.playerId
            )
            _ <- performStartGame(
              startGameRequest(
                hostWelcome,
                Some(initialStack),
                None,
                Some(timerConfig),
                playerOrder
              ),
              context(hostAddress)
            )
            playerDbs <- db.getPlayers(hostWelcome.gameId)
          } yield playerDbs
            .map(pdb => pdb.stack + pdb.bet)
            .distinct shouldEqual List(initialStack)
      }

      "player blind payments should be persisted" in appContextRes.use {
        (context, db) =>
          for {
            (hostWelcome, p1Welcome, p2Welcome) <- gameFixture(context)
            playerOrder = List(
              hostWelcome.playerId,
              p1Welcome.playerId,
              p2Welcome.playerId
            )
            _ <- performStartGame(
              startGameRequest(
                hostWelcome,
                Some(initialStack),
                None,
                Some(timerConfig),
                playerOrder
              ),
              context(hostAddress)
            )
            playerDbs <- db.getPlayers(hostWelcome.gameId)
          } yield {
            // order playerdb results to match game order
            playerDbs
              .sortBy(pdb => playerOrder.map(_.pid).indexOf(pdb.playerId))
              .map(pdb => pdb.bet) shouldEqual List(0, 5, 10)
          }
      }
    }

    "fails if neither timer config nor initial small blind are provided" in appContextRes.use {
      (context, _) =>
        val result = for {
          (hostWelcome, p1Welcome, p2Welcome) <- gameFixture(context)
          playerOrder = List(
            hostWelcome.playerId,
            p1Welcome.playerId,
            p2Welcome.playerId
          )
          _ <- performStartGame(
            startGameRequest(
              hostWelcome,
              Some(initialStack),
              None,
              None,
              playerOrder
            ),
            context(hostAddress)
          )
        } yield ()
        result.assertThrows[Failures]
    }
  }

  "fails, when" - {
    "the game has already started" in appContextRes.use { (context, _) =>
      val result = for {
        (hostWelcome, p1Welcome, p2Welcome) <- gameFixture(context)
        playerOrder = List(
          hostWelcome.playerId,
          p1Welcome.playerId,
          p2Welcome.playerId
        )
        _ <- performStartGame(
          startGameRequest(hostWelcome, None, None, None, playerOrder),
          context(hostAddress)
        ).assertNoException
        _ <- performStartGame(
          startGameRequest(hostWelcome, None, None, None, playerOrder),
          context(hostAddress)
        )
      } yield ()
      result.assertThrows[Failures]
    }

    "the request is not a valid start game request" in appContextRes.use {
      (context, _) =>
        val result = for {
          (hostWelcome, p1Welcome, p2Welcome) <- gameFixture(context)
          playerOrder = List(
            hostWelcome.playerId,
            p1Welcome.playerId,
            p2Welcome.playerId
          )
          _ <- performStartGame("""{"foo":"bar"}""", context(hostAddress))
        } yield ()
        result.assertThrows[Failures]
    }

    "the player making the call is not the host" in appContextRes.use {
      (context, _) =>
        val result = for {
          (hostWelcome, p1Welcome, p2Welcome) <- gameFixture(context)
          playerOrder = List(
            hostWelcome.playerId,
            p1Welcome.playerId,
            p2Welcome.playerId
          )
          _ <- performStartGame(
            startGameRequest(p1Welcome, None, None, None, playerOrder),
            context(player1Address)
          )
        } yield ()
        result.assertThrows[Failures]
    }

    "this player has not joined this game" in appContextRes.use { (context, _) =>
      val result = for {
        (hostWelcome, p1Welcome, p2Welcome) <- gameFixture(context)
        playerOrder = List(
          hostWelcome.playerId,
          p1Welcome.playerId,
          p2Welcome.playerId
        )
        playerAddress = PlayerAddress("another-address")
        request = StartGame(
          hostWelcome.gameId,
          PlayerId("different-id"),
          PlayerKey("different-key"),
          None,
          None,
          None,
          playerOrder
        )
        _ <- performStartGame(
          encodeRequest(request).noSpaces,
          context(playerAddress)
        )
      } yield ()
      result.assertThrows[Failures]
    }
  }

  private def gameFixture(
      contextBuilder: PlayerAddress => AppContext[IO]
  )(implicit pos: Position): IO[(Welcome, Welcome, Welcome)] = {
    for {
      hostResponse <- performCreateGame(
        createGameRequest,
        contextBuilder(hostAddress),
        initialSeed
      )
      hostWelcome = hostResponse.messages.get(hostAddress).value
      gameCode = hostWelcome.gameCode
      p1JoinResponse <- performJoinGame(
        joinGameRequest(gameCode, "player-1"),
        contextBuilder(player1Address)
      )
      p1Welcome = p1JoinResponse.messages.get(player1Address).value
      p2JoinResponse <- performJoinGame(
        joinGameRequest(gameCode, "player-2"),
        contextBuilder(player2Address)
      )
      p2Welcome = p2JoinResponse.messages.get(player2Address).value
    } yield (hostWelcome, p1Welcome, p2Welcome)
  }
}

object StartGameIntegrationTest {
  def startGameRequest(
      welcome: Welcome,
      startingStack: Option[Int],
      initialSmallBlind: Option[Int],
      timerConfig: Option[List[TimerLevel]],
      playerOrder: List[PlayerId]
  ): String = {
    val request = StartGame(
      welcome.gameId,
      welcome.playerId,
      welcome.playerKey,
      startingStack,
      initialSmallBlind,
      timerConfig,
      playerOrder
    )
    encodeRequest(request).noSpaces
  }

  def performStartGame(
      request: String,
      appContext: AppContext[IO]
  ): IO[Response[GameStatus]] = {
    PokerDot.startGame(parseReq(request), appContext)
  }
}
