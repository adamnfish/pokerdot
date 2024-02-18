package io.adamnfish.pokerdot.validation

import io.adamnfish.pokerdot.models.*
import io.adamnfish.pokerdot.validation.Validation.{extractAdvancePhase, extractBet, extractCheck, extractCreateGame, extractFold, extractJoinGame, extractPing, extractStartGame, extractUpdateBlind, validate}
import io.circe.parser.parse
import org.scalacheck.Gen
import org.scalatest.{EitherValues, TryValues}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import java.util.UUID
import scala.util.Try


class ValidationTest extends AnyFreeSpec with Matchers with EitherValues with TryValues with ScalaCheckDrivenPropertyChecks {
  val gameId = UUID.randomUUID().toString
  val player1Id = UUID.randomUUID().toString
  val player2Id = UUID.randomUUID().toString
  val player3Id = UUID.randomUUID().toString
  val playerKey = UUID.randomUUID().toString

  "extractCreateGame" in {
    val jsonStr = """{"operation":"create-game","screenName":"screen name","gameName":"game name"}"""
    val json = parse(jsonStr).value
    extractCreateGame[Try](json).success.value shouldEqual CreateGame(
      screenName = "screen name",
      gameName = "game name",
    )
  }

  "extractJoinGame" in {
    val jsonStr = """{"operation":"join-game","gameCode":"abcd","screenName":"screen name"}"""
    val json = parse(jsonStr).value
    extractJoinGame[Try](json).success.value shouldEqual JoinGame(
      gameCode = "abcd",
      screenName = "screen name",
    )
  }

  "extractStartGame" - {
    "with timer" in {
      val jsonStr =
        s"""{"operation":"start-game","gameId":"$gameId","playerId":"$player1Id","playerKey":"$playerKey","playerOrder":["$player1Id","$player2Id","$player3Id"],
           |"timerConfig":[{"durationSeconds":300,"smallBlind":5},{"durationSeconds":60},{"durationSeconds":500,"smallBlind":10}]}""".stripMargin
      val json = parse(jsonStr).value
      extractStartGame[Try](json).success.value shouldEqual StartGame(
        GameId(gameId), PlayerId(player1Id), PlayerKey(playerKey),
        startingStack = None,
        initialSmallBlind = None,
        timerConfig = Some(List(RoundLevel(300, 5), BreakLevel(60), RoundLevel(500, 10))),
        playerOrder = List(PlayerId(player1Id), PlayerId(player2Id), PlayerId(player3Id))
      )
    }

    "with stacks and small blind" in {
      val jsonStr =
        s"""{"operation":"start-game","gameId":"$gameId","playerId":"$player1Id","playerKey":"$playerKey","playerOrder":["$player1Id","$player2Id","$player3Id"],
           |"startingStack":100,"initialSmallBlind":1}""".stripMargin
      val json = parse(jsonStr).value
      extractStartGame[Try](json).success.value shouldEqual StartGame(
        GameId(gameId), PlayerId(player1Id), PlayerKey(playerKey),
        startingStack = Some(100),
        initialSmallBlind = Some(1),
        timerConfig = None,
        playerOrder = List(PlayerId(player1Id), PlayerId(player2Id), PlayerId(player3Id))
      )
    }

    "with timer and stacks" in {
      val jsonStr =
        s"""{"operation":"start-game","gameId":"$gameId","playerId":"$player1Id","playerKey":"$playerKey","playerOrder":["$player1Id","$player2Id","$player3Id"],
           |"timerConfig":[{"durationSeconds":300,"smallBlind":5},{"durationSeconds":60},{"durationSeconds":500,"smallBlind":10}],
           |"startingStack":100}""".stripMargin
      val json = parse(jsonStr).value
      extractStartGame[Try](json).success.value shouldEqual StartGame(
        GameId(gameId), PlayerId(player1Id), PlayerKey(playerKey),
        startingStack = Some(100),
        initialSmallBlind = None,
        timerConfig = Some(List(RoundLevel(300, 5), BreakLevel(60), RoundLevel(500, 10))),
        playerOrder = List(PlayerId(player1Id), PlayerId(player2Id), PlayerId(player3Id))
      )
    }
  }

  "extractBet" in {
    val jsonStr =
      s"""{"operation":"bet","gameId":"$gameId","playerId":"$player1Id","playerKey":"$playerKey",
         |"betAmount":100}""".stripMargin
    val json = parse(jsonStr).value
    extractBet[Try](json).success.value shouldEqual Bet(
      GameId(gameId), PlayerKey(playerKey), PlayerId(player1Id),
      100,
    )
  }

  "extractCheck" in {
    val jsonStr =
      s"""{"operation":"check","gameId":"$gameId","playerId":"$player1Id","playerKey":"$playerKey"}""".stripMargin
    val json = parse(jsonStr).value
    extractCheck[Try](json).success.value shouldEqual Check(
      GameId(gameId), PlayerKey(playerKey), PlayerId(player1Id),
    )
  }

  "extractFold" in {
    val jsonStr =
      s"""{"operation":"fold","gameId":"$gameId","playerId":"$player1Id","playerKey":"$playerKey"}""".stripMargin
    val json = parse(jsonStr).value
    extractFold[Try](json).success.value shouldEqual Fold(
      GameId(gameId), PlayerKey(playerKey), PlayerId(player1Id),
    )
  }

  "extractAdvancePhase" in {
    val jsonStr =
      s"""{"operation":"advance-phase","gameId":"$gameId","playerId":"$player1Id","playerKey":"$playerKey"}""".stripMargin
    val json = parse(jsonStr).value
    extractAdvancePhase[Try](json).success.value shouldEqual AdvancePhase(
      GameId(gameId), PlayerKey(playerKey), PlayerId(player1Id),
    )
  }

  "extractUpdateBlind" - {
    "with timer levels" in {
      val jsonStr =
        s"""{"operation":"update-blind","gameId":"$gameId","playerId":"$player1Id","playerKey":"$playerKey",
           |"timerLevels":[{"durationSeconds":300,"smallBlind":5},{"durationSeconds":60},{"durationSeconds":500,"smallBlind":10}],
           |"playing":true}""".stripMargin
      val json = parse(jsonStr).value
      extractUpdateBlind[Try](json).success.value shouldEqual UpdateBlind(
        GameId(gameId), PlayerId(player1Id), PlayerKey(playerKey),
        timerLevels = Some(List(RoundLevel(300, 5), BreakLevel(60), RoundLevel(500, 10))),
        smallBlind = None,
        playing = Some(true),
        progress = None,
      )
    }

    "playing status update" in {
      val jsonStr =
        s"""{"operation":"update-blind","gameId":"$gameId","playerId":"$player1Id","playerKey":"$playerKey",
           |"playing":true}""".stripMargin
      val json = parse(jsonStr).value
      extractUpdateBlind[Try](json).success.value shouldEqual UpdateBlind(
        GameId(gameId), PlayerId(player1Id), PlayerKey(playerKey),
        timerLevels = None,
        smallBlind = None,
        playing = Some(true),
        progress = None,
      )
    }

    "progress update" - {
      "works for a valid progress update request" in {
        val jsonStr =
          s"""{"operation":"update-blind","gameId":"$gameId","playerId":"$player1Id","playerKey":"$playerKey",
             |"progress":350}""".stripMargin
        val json = parse(jsonStr).value
        extractUpdateBlind[Try](json).success.value shouldEqual UpdateBlind(
          GameId(gameId), PlayerId(player1Id), PlayerKey(playerKey),
          timerLevels = None,
          smallBlind = None,
          playing = None,
          progress = Some(350),
        )
      }

      "fails for a negative progress update" in {
        val jsonStr =
          s"""{"operation":"update-blind","gameId":"$gameId","playerId":"$player1Id","playerKey":"$playerKey",
             |"progress":-10}""".stripMargin
        val json = parse(jsonStr).value
        extractUpdateBlind[Try](json).isFailure shouldEqual true
      }
    }

    "with manual blind change" in {
      val jsonStr =
        s"""{"operation":"update-blind","gameId":"$gameId","playerId":"$player1Id","playerKey":"$playerKey",
           |"smallBlind":50}""".stripMargin
      val json = parse(jsonStr).value
      extractUpdateBlind[Try](json).success.value shouldEqual UpdateBlind(
        GameId(gameId), PlayerId(player1Id), PlayerKey(playerKey),
        timerLevels = None,
        smallBlind = Some(50),
        playing = None,
        progress = None,
      )
    }
  }

  "extractPing" - {
    val jsonStr =
      s"""{"operation":"ping","gameId":"$gameId","playerId":"$player1Id","playerKey":"$playerKey"}""".stripMargin
    val json = parse(jsonStr).value
    extractPing[Try](json).success.value shouldEqual Ping(
      GameId(gameId), PlayerId(player1Id), PlayerKey(playerKey),
    )
  }

  "validate CreateGame" - {
    "returns the request for a valid create game request" in {
      val request = CreateGame("screen name", "game name")
      validate[Try](request).success.value shouldEqual request
    }

    "returns a failure if the screen name is empty" in {
      validate[Try](CreateGame("", "game name")).isFailure shouldEqual true
    }

    "returns a failure if the screen name is very long" in {
      validate[Try](CreateGame("a" * 60, "game name")).isFailure shouldEqual true
    }

    "returns a failure if the game name is empty" in {
      validate[Try](CreateGame("screen name", "")).isFailure shouldEqual true
    }

    "returns a failure if the game name is very long" in {
      validate[Try](CreateGame("screen name", "a" * 60)).isFailure shouldEqual true
    }
  }

  "validate JoinGame" - {
    "returns the request for a valid join game request" in {
      val request = JoinGame("abcde", "screen name")
      validate[Try](request).success.value shouldEqual request
    }

    "returns a failure if the game code is empty" in {
      validate[Try](JoinGame("", "game name")).isFailure shouldEqual true
    }

    "returns a failure if the game code doesn't look like a game code" in {
      validate[Try](JoinGame("n -ot A! gameCode", "game name")).isFailure shouldEqual true
    }

    "returns a failure if the screen name is empty" in {
      validate[Try](JoinGame("abcde", "")).isFailure shouldEqual true
    }

    "returns a failure if the screen name is very long" in {
      validate[Try](JoinGame("abcde", "a" * 60)).isFailure shouldEqual true
    }
  }

  "validate StartGame" - {
    val timerExample = List(RoundLevel(300, 1), BreakLevel(60), RoundLevel(300, 2))
    val rawRequest = StartGame(
      GameId(gameId), PlayerId(player1Id), PlayerKey(playerKey),
      None, None, None,
      List(PlayerId(player1Id), PlayerId(player2Id), PlayerId(player3Id))
    )

    "for valid requests" - {
      "with no stack information" in {
        val request = rawRequest.copy(
          startingStack = None,
          initialSmallBlind = None,
          timerConfig = None,
        )
        validate[Try](request).success.value shouldEqual request
      }

      "with timer but no stack information" in {
        val request = rawRequest.copy(
          startingStack = None,
          initialSmallBlind = None,
          timerConfig = Some(timerExample),
        )
        validate[Try](request).success.value shouldEqual request
      }

      "with stack and timer information" in {
        val request = rawRequest.copy(
          startingStack = Some(1000),
          timerConfig = Some(timerExample),
          initialSmallBlind = None,
        )
        validate[Try](request).success.value shouldEqual request
      }

      "with stack and small blind information" in {
        val request = rawRequest.copy(
          startingStack = Some(1000),
          timerConfig = None,
          initialSmallBlind = Some(1),
        )
        validate[Try](request).success.value shouldEqual request
      }
    }

    "returns a failure if the game id is not valid" in {
      validate[Try](rawRequest.copy(gameId = GameId("invalid!"))).isFailure shouldEqual true
    }

    "returns a failure if the player id is not valid" in {
      validate[Try](rawRequest.copy(playerId = PlayerId("invalid!"))).isFailure shouldEqual true
    }

    "returns a failure if the player key is not valid" in {
      validate[Try](rawRequest.copy(playerKey = PlayerKey("invalid!"))).isFailure shouldEqual true
    }

    "returns a failure if player order is empty" in {
      validate[Try](rawRequest.copy(playerOrder = Nil)).isFailure shouldEqual true
    }

    "if the game is tracking stacks" - {
      "if the game is tracking stacks, fails if there is neither a timer config nor an initial stack amount" in {
        val request = rawRequest.copy(
          startingStack = Some(1000),
          timerConfig = None,
          initialSmallBlind = None,
        )
        validate[Try](request).isFailure shouldEqual true
      }

      "if the game is tracking stacks, fails if both timer config and initial stack amount are provided" in {
        val request = rawRequest.copy(
          startingStack = Some(1000),
          timerConfig = Some(timerExample),
          initialSmallBlind = Some(1),
        )
        validate[Try](request).isFailure shouldEqual true
      }

      "fails if stacks are 0" in {
        val request = rawRequest.copy(
          startingStack = Some(0),
          timerConfig = None,
          initialSmallBlind = Some(10),
        )
        validate[Try](request).isFailure shouldEqual true
      }

      "fails if initial blind is 0" in {
        val request = rawRequest.copy(
          startingStack = Some(1000),
          timerConfig = None,
          initialSmallBlind = Some(0),
        )
        validate[Try](request).isFailure shouldEqual true
      }

    }
  }

  "validate UpdateBlind" - {
    val rawRequest = UpdateBlind(
      GameId(gameId), PlayerId(player1Id), PlayerKey(playerKey),
      timerLevels = None,
      smallBlind = None,
      playing = None,
      progress = None,
    )

    "returns the request for a valid update timer requests" - {
      "with timer levels" in {
        val timerLevelsRequest = rawRequest.copy(timerLevels = Some(List(RoundLevel(300, 1), BreakLevel(60), RoundLevel(300, 2))))
        validate[Try](timerLevelsRequest).success.value shouldEqual timerLevelsRequest
      }

      "without timer levels" in {
        val requestWithoutTimerLevels = rawRequest.copy(timerLevels = None, smallBlind = Some(10))
        validate[Try](requestWithoutTimerLevels).success.value shouldEqual requestWithoutTimerLevels
      }

      "with manual blind update" in {
        val requestWithSmallBlindAmount = rawRequest.copy(smallBlind = Some(50))
        validate[Try](requestWithSmallBlindAmount).success.value shouldEqual requestWithSmallBlindAmount
      }

      "with a new timer progress" in {
        val requestWithTimerProgress = rawRequest.copy(progress = Some(500))
        validate[Try](requestWithTimerProgress).success.value shouldEqual requestWithTimerProgress
      }
    }

    "returns a failure if the game id is not valid" in {
      validate[Try](rawRequest.copy(gameId = GameId("invalid!"))).isFailure shouldEqual true
    }

    "returns a failure if the player id is not valid" in {
      validate[Try](rawRequest.copy(playerId = PlayerId("invalid!"))).isFailure shouldEqual true
    }

    "returns a failure if the player key is not valid" in {
      validate[Try](rawRequest.copy(playerKey = PlayerKey("invalid!"))).isFailure shouldEqual true
    }

    "returns a failure if the timer levels are present and empty" in {
      validate[Try](rawRequest.copy(timerLevels = Some(Nil))).isFailure shouldEqual true
    }

    "returns a failure if the update blind request is 'empty'" in {
      validate[Try](rawRequest).isFailure shouldEqual true
    }
  }

  "validate Bet" - {
    val request = Bet(
      GameId(gameId), PlayerKey(playerKey), PlayerId(player1Id),
      100,
    )

    "returns the request for a valid bet request" in {
      validate[Try](request).success.value shouldEqual request
    }

    "returns a failure if the game id is not valid" in {
      validate[Try](request.copy(gameId = GameId("invalid!"))).isFailure shouldEqual true
    }

    "returns a failure if the player id is not valid" in {
      validate[Try](request.copy(playerId = PlayerId("invalid!"))).isFailure shouldEqual true
    }

    "returns a failure if the player key is not valid" in {
      validate[Try](request.copy(playerKey = PlayerKey("invalid!"))).isFailure shouldEqual true
    }

    "returns a failure if bet amount is 0" in {
      validate[Try](request.copy(betAmount = 0)).isFailure shouldEqual true
    }

    "returns a failure if bet amount is -ve" in {
      forAll(Gen.negNum[Int]) { betAmount =>
        validate[Try](request.copy(betAmount = betAmount)).isFailure shouldEqual true
      }
    }
  }

  "validate Check" - {
    val request = Check(
      GameId(gameId), PlayerKey(playerKey), PlayerId(player1Id),
    )

    "returns the request for a valid bet request" in {
      validate[Try](request).success.value shouldEqual request
    }

    "returns a failure if the game id is not valid" in {
      validate[Try](request.copy(gameId = GameId("invalid!"))).isFailure shouldEqual true
    }

    "returns a failure if the player id is not valid" in {
      validate[Try](request.copy(playerId = PlayerId("invalid!"))).isFailure shouldEqual true
    }

    "returns a failure if the player key is not valid" in {
      validate[Try](request.copy(playerKey = PlayerKey("invalid!"))).isFailure shouldEqual true
    }
  }

  "validate Fold" - {
    val request = Fold(
      GameId(gameId), PlayerKey(playerKey), PlayerId(player1Id),
    )

    "returns the request for a valid bet request" in {
      validate[Try](request).success.value shouldEqual request
    }

    "returns a failure if the game id is not valid" in {
      validate[Try](request.copy(gameId = GameId("invalid!"))).isFailure shouldEqual true
    }

    "returns a failure if the player id is not valid" in {
      validate[Try](request.copy(playerId = PlayerId("invalid!"))).isFailure shouldEqual true
    }

    "returns a failure if the player key is not valid" in {
      validate[Try](request.copy(playerKey = PlayerKey("invalid!"))).isFailure shouldEqual true
    }
  }

  "validate AdvancePhase" - {
    val request = AdvancePhase(
      GameId(gameId), PlayerKey(playerKey), PlayerId(player1Id),
    )

    "returns the request for a valid bet request" in {
      validate[Try](request).success.value shouldEqual request
    }

    "returns a failure if the game id is not valid" in {
      validate[Try](request.copy(gameId = GameId("invalid!"))).isFailure shouldEqual true
    }

    "returns a failure if the player id is not valid" in {
      validate[Try](request.copy(playerId = PlayerId("invalid!"))).isFailure shouldEqual true
    }

    "returns a failure if the player key is not valid" in {
      validate[Try](request.copy(playerKey = PlayerKey("invalid!"))).isFailure shouldEqual true
    }
  }

  "validate Ping" - {
    val request = Ping(
      GameId(gameId), PlayerId(player1Id), PlayerKey(playerKey),
    )

    "returns the request for a valid bet request" in {
      validate[Try](request).success.value shouldEqual request
    }

    "returns a failure if the game id is not valid" in {
      validate[Try](request.copy(gameId = GameId("invalid!"))).isFailure shouldEqual true
    }

    "returns a failure if the player id is not valid" in {
      validate[Try](request.copy(playerId = PlayerId("invalid!"))).isFailure shouldEqual true
    }

    "returns a failure if the player key is not valid" in {
      validate[Try](request.copy(playerKey = PlayerKey("invalid!"))).isFailure shouldEqual true
    }
  }
}
