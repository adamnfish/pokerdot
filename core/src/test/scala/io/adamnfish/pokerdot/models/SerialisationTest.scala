package io.adamnfish.pokerdot.models

import io.adamnfish.pokerdot.TestHelpers
import io.adamnfish.pokerdot.models.Serialisation.parseUpdateTimerRequest
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers


class SerialisationTest extends AnyFreeSpec with Matchers with TestHelpers {
  "parseUpdateTimerRequest" - {
    "parses a pause request" in {
      val json = parseReq(
        """{"gameId": "gid",
          | "playerId": "pid",
          | "playerKey": "pkey",
          | "timerLevels": null,
          | "playing": false
          |}""".stripMargin
      )
      parseUpdateTimerRequest(json).value should have(
        "gameId" as "gid",
        "playerId" as "pid",
        "playerKey" as "pkey",
        "timerLevels" as None,
        "playing" as false,
      )
    }

    "parses a pause request with a missing timerLevels key" in {
      val json = parseReq(
        """{"gameId": "gid",
          | "playerId": "pid",
          | "playerKey": "pkey",
          | "playing": false
          |}""".stripMargin
      )
      parseUpdateTimerRequest(json).value should have(
        "gameId" as "gid",
        "playerId" as "pid",
        "playerKey" as "pkey",
        "timerLevels" as None,
        "playing" as false,
      )
    }

    "parses a play request" in {
      val json = parseReq(
        """{"gameId": "gid",
          | "playerId": "pid",
          | "playerKey": "pkey",
          | "timerLevels": null,
          | "playing": true
          |}""".stripMargin
      )
      parseUpdateTimerRequest(json).value should have(
        "gameId" as "gid",
        "playerId" as "pid",
        "playerKey" as "pkey",
        "timerLevels" as None,
        "playing" as true,
      )
    }

    "parses a play request with a missing timerLevels key" in {
      val json = parseReq(
        """{"gameId": "gid",
          | "playerId": "pid",
          | "playerKey": "pkey",
          | "playing": true
          |}""".stripMargin
      )
      parseUpdateTimerRequest(json).value should have(
        "gameId" as "gid",
        "playerId" as "pid",
        "playerKey" as "pkey",
        "timerLevels" as None,
        "playing" as true,
      )
    }

    "parses an edit request" in {
      val json = parseReq(
        """{"gameId": "gid",
          | "playerId": "pid",
          | "playerKey": "pkey",
          | "timerLevels": [
          |  {"durationSeconds": 300, "smallBlind": 5},
          |  {"durationSeconds": 45},
          |  {"durationSeconds": 200, "smallBlind": 10}
          | ],
          | "playing": true
          |}""".stripMargin
      )
      parseUpdateTimerRequest(json).value should have(
        "gameId" as "gid",
        "playerId" as "pid",
        "playerKey" as "pkey",
        "timerLevels" as Some(List(
          RoundLevel(300, 5), BreakLevel(45), RoundLevel(200, 10)
        )),
        "playing" as true,
      )
    }
  }
}
