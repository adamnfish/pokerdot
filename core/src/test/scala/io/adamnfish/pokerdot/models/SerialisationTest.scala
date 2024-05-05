package io.adamnfish.pokerdot.models

import io.adamnfish.pokerdot.TestHelpers
import io.adamnfish.pokerdot.TestHelpers.parseReq
import io.adamnfish.pokerdot.logic.Cards.RichRank
import io.adamnfish.pokerdot.models.Serialisation.{parseUpdateBlindRequest, _}
import io.circe.Json
import io.circe.generic.semiauto.deriveDecoder
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import io.circe.syntax._
import org.scalatest.EitherValues


class SerialisationTest extends AnyFreeSpec with Matchers with TestHelpers {
  "parse" - {
    "for invalid input" - {
      "fails" in {
        parse("""nope""", "Test message", None).isLeft shouldEqual true
      }

      "uses the provided message in the failure" in {
        val failures = parse("""nope""", "Test message", None).leftValue
        failures.failures.exists(_.userMessage == "Test message") shouldEqual true
      }

      "uses the provided context in the failure" in {
        val failures = parse("""nope""", "Test message", Some("context")).leftValue
        failures.failures.exists(_.context.contains("context")) shouldEqual true
      }
    }
  }

  "extractJson" - {
    case class Test(field: String)
    implicit val testDecoder = deriveDecoder[Test]

    "succeeds if the JSON is valid" in {
      val result = extractJson(Json.fromFields(List(("field", Json.fromString("value")))), "Test message")
      result.value shouldEqual Test(field = "value")
    }

    "fails if the JSON is not in the correct shape" in {
      val result = extractJson(Json.fromFields(List(("differentField", Json.fromString("value")))), "Test message")
      result.isLeft shouldEqual true
    }
  }

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
      parseUpdateBlindRequest(json).value should have(
        "gameId" as "gid",
        "playerId" as "pid",
        "playerKey" as "pkey",
        "timerLevels" as None,
        "playing" as Some(false),
        "smallBlind" as None,
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
      parseUpdateBlindRequest(json).value should have(
        "gameId" as "gid",
        "playerId" as "pid",
        "playerKey" as "pkey",
        "timerLevels" as None,
        "playing" as Some(false),
        "smallBlind" as None,
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
      parseUpdateBlindRequest(json).value should have(
        "gameId" as "gid",
        "playerId" as "pid",
        "playerKey" as "pkey",
        "timerLevels" as None,
        "playing" as Some(true),
        "smallBlind" as None,
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
      parseUpdateBlindRequest(json).value should have(
        "gameId" as "gid",
        "playerId" as "pid",
        "playerKey" as "pkey",
        "timerLevels" as None,
        "playing" as Some(true),
        "smallBlind" as None,
      )
    }

    "parses an edit request" in {
      val json = parseReq(
        """{"gameId": "gid",
          | "playerId": "pid",
          | "playerKey": "pkey",
          | "timerLevels": [
          |    {"durationSeconds": 300, "smallBlind": 5},
          |    {"durationSeconds": 45},
          |    {"durationSeconds": 200, "smallBlind": 10}
          | ],
          | "playing": true
          |}""".stripMargin
      )
      parseUpdateBlindRequest(json).value should have(
        "gameId" as "gid",
        "playerId" as "pid",
        "playerKey" as "pkey",
        "timerLevels" as Some(List(
          RoundLevel(300, 5), BreakLevel(45), RoundLevel(200, 10)
        )),
        "playing" as Some(true),
        "smallBlind" as None,
      )
    }

    "parses a manual update to the blind level" in {

      val json = parseReq(
        """{"gameId": "gid",
          | "playerId": "pid",
          | "playerKey": "pkey",
          | "smallBlind": 25
          |}""".stripMargin
      )
      parseUpdateBlindRequest(json).value should have(
        "gameId" as "gid",
        "playerId" as "pid",
        "playerKey" as "pkey",
        "timerLevels" as None,
        "playing" as None,
        "smallBlind" as Some(25),
      )
    }
  }

  "handEncoder" - {
    "highCard encoding includes the correct hand name" in {
      val hand: Hand = HighCard(Two of Hearts, Three of Clubs, Four of Spades, Ten of Diamonds, Queen of Clubs)
      hand.asJson.hcursor.downField("hand").as[String].value shouldEqual "high-card"
    }

    "pair encoding includes the correct hand name" in {
      val hand: Hand = Pair(Two of Hearts, Three of Clubs, Four of Spades, Ten of Diamonds, Queen of Clubs)
      hand.asJson.hcursor.downField("hand").as[String].value shouldEqual "pair"
    }

    "twoPair encoding includes the correct hand name" in {
      val hand: Hand = TwoPair(Two of Hearts, Three of Clubs, Four of Spades, Ten of Diamonds, Queen of Clubs)
      hand.asJson.hcursor.downField("hand").as[String].value shouldEqual "two-pair"
    }

    "threeOfAKind encoding includes the correct hand name" in {
      val hand: Hand = ThreeOfAKind(Two of Hearts, Three of Clubs, Four of Spades, Ten of Diamonds, Queen of Clubs)
      hand.asJson.hcursor.downField("hand").as[String].value shouldEqual "three-of-a-kind"
    }

    "straight encoding includes the correct hand name" in {
      val hand: Hand = Straight(Two of Hearts, Three of Clubs, Four of Spades, Ten of Diamonds, Queen of Clubs)
      hand.asJson.hcursor.downField("hand").as[String].value shouldEqual "straight"
    }

    "flush encoding includes the correct hand name" in {
      val hand: Hand = Flush(Two of Hearts, Three of Clubs, Four of Spades, Ten of Diamonds, Queen of Clubs)
      hand.asJson.hcursor.downField("hand").as[String].value shouldEqual "flush"
    }

    "fullHouse encoding includes the correct hand name" in {
      val hand: Hand = FullHouse(Two of Hearts, Three of Clubs, Four of Spades, Ten of Diamonds, Queen of Clubs)
      hand.asJson.hcursor.downField("hand").as[String].value shouldEqual "full-house"
    }

    "fourOfAKind encoding includes the correct hand name" in {
      val hand: Hand = FourOfAKind(Two of Hearts, Three of Clubs, Four of Spades, Ten of Diamonds, Queen of Clubs)
      hand.asJson.hcursor.downField("hand").as[String].value shouldEqual "four-of-a-kind"
    }

    "straightFlush encoding includes the correct hand name" in {
      val hand: Hand = StraightFlush(Two of Hearts, Three of Clubs, Four of Spades, Ten of Diamonds, Queen of Clubs)
      hand.asJson.hcursor.downField("hand").as[String].value shouldEqual "straight-flush"
    }
  }

  "roundSummaryEncoder" - {
    "for pre-flop includes the correct phase name" in {
      val round: RoundSummary = PreFlopSummary()
      round.asJson.hcursor.downField("phase").as[String].value shouldEqual "pre-flop"
    }

    "for flop includes the correct phase name" in {
      val round: RoundSummary = FlopSummary(Two of Hearts, Three of Clubs, Four of Spades)
      round.asJson.hcursor.downField("phase").as[String].value shouldEqual "flop"
    }

    "for turn includes the correct phase name" in {
      val round: RoundSummary = TurnSummary(Two of Hearts, Three of Clubs, Four of Spades, Ten of Diamonds)
      round.asJson.hcursor.downField("phase").as[String].value shouldEqual "turn"
    }

    "for river includes the correct phase name" in {
      val round: RoundSummary = RiverSummary(Two of Hearts, Three of Clubs, Four of Spades, Ten of Diamonds, Queen of Clubs)
      round.asJson.hcursor.downField("phase").as[String].value shouldEqual "river"
    }

    "for showdown includes the correct phase name" in {
      val round: RoundSummary = ShowdownSummary(Two of Hearts, Three of Clubs, Four of Spades, Ten of Diamonds, Queen of Clubs, Nil)
      round.asJson.hcursor.downField("phase").as[String].value shouldEqual "showdown"
    }
  }

  "actionSummaryEncoder" - {
    val playerId = PlayerId("pid")

    "gameStartedSummary encoding includes correct the action name" in {
      val actionSummary: ActionSummary = GameStartedSummary()
      actionSummary.asJson.hcursor.downField("action").as[String].value shouldEqual "game-started"
    }

    "playerJoinedSummary encoding includes correct the action name" in {
      val actionSummary: ActionSummary = PlayerJoinedSummary(playerId)
      actionSummary.asJson.hcursor.downField("action").as[String].value shouldEqual "player-joined"
    }

    "betSummary encoding includes correct the action name" in {
      val actionSummary: ActionSummary = BetSummary(playerId, 10)
      actionSummary.asJson.hcursor.downField("action").as[String].value shouldEqual "bet"
    }

    "checkSummary encoding includes correct the action name" in {
      val actionSummary: ActionSummary = CheckSummary(playerId)
      actionSummary.asJson.hcursor.downField("action").as[String].value shouldEqual "check"
    }

    "foldSummary encoding includes correct the action name" in {
      val actionSummary: ActionSummary = FoldSummary(playerId)
      actionSummary.asJson.hcursor.downField("action").as[String].value shouldEqual "fold"
    }

    "advancePhaseSummary encoding includes correct the action name" in {
      val actionSummary: ActionSummary = AdvancePhaseSummary()
      actionSummary.asJson.hcursor.downField("action").as[String].value shouldEqual "advance-phase"
    }

    "timerStatusSummary encoding includes correct the action name" in {
      val actionSummary: ActionSummary = TimerStatusSummary(true)
      actionSummary.asJson.hcursor.downField("action").as[String].value shouldEqual "timer-status"
    }

    "editBlindSummary encoding includes correct the action name" in {
      val actionSummary: ActionSummary = EditBlindSummary()
      actionSummary.asJson.hcursor.downField("action").as[String].value shouldEqual "edit-blind"
    }

    "noActionSummary encoding includes correct the action name" in {
      val actionSummary: ActionSummary = NoActionSummary()
      actionSummary.asJson.hcursor.downField("action").as[String].value shouldEqual "no-action"
    }
  }
}
