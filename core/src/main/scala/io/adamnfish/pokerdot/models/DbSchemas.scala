package io.adamnfish.pokerdot.models

import dynosaur._
import cats.syntax.all._
import io.adamnfish.pokerdot.logic.{Cards, Representations}


object DbSchemas {
  val rankSchema: Schema[Rank] = Schema[String].imapErr {
    case "2" =>
      Right(Two)
    case "3" =>
      Right(Three)
    case "4" =>
      Right(Four)
    case "5" =>
      Right(Five)
    case "6" =>
      Right(Six)
    case "7" =>
      Right(Seven)
    case "8" =>
      Right(Eight)
    case "9" =>
      Right(Nine)
    case "10" =>
      Right(Ten)
    case "J" =>
      Right(Jack)
    case "Q" =>
      Right(Queen)
    case "K" =>
      Right(King)
    case "A" =>
      Right(Ace)
    case otherRank =>
      Left(Schema.ReadError(s"Invalid rank: $otherRank"))
  }(Cards.rankStr)

  val suitSchema: Schema[Suit] = Schema[String].imapErr {
    case "♣" =>
      Right(Clubs)
    case "♦" =>
      Right(Diamonds)
    case "♥" =>
      Right(Hearts)
    case "♠" =>
      Right(Spades)
    case otherSuit =>
      Left(Schema.ReadError(s"Invalid suit: $otherSuit"))
  }(Cards.suitStr)

  val cardSchema: Schema[Card] = Schema.record[Card] { field =>
    (
      field("rank", _.rank)(rankSchema),
      field("suit", _.suit)(suitSchema),
    ).mapN(Card.apply)
  }

  val holeSchema: Schema[Hole] = Schema.record[Hole] { field =>
    (
      field("card1", _.card1)(cardSchema),
      field("card2", _.card2)(cardSchema)
    ).mapN(Hole.apply)
  }

  val roundLevelSchema: Schema[RoundLevel] = Schema.record[RoundLevel] { field =>
    (
      field("durationSeconds", _.durationSeconds)(Schema[Int]),
      field("smallBlind", _.smallBlind)(Schema[Int])
    ).mapN(RoundLevel.apply)
  }.tag("round")

  val breakLevelSchema: Schema[BreakLevel] = Schema.record[BreakLevel] { field =>
    field("durationSeconds", _.durationSeconds)(Schema[Int])
      .map(BreakLevel.apply)
  }.tag("break")

  val timerLevelSchema: Schema[TimerLevel] = Schema.oneOf { alt =>
    alt(roundLevelSchema) |+| alt(breakLevelSchema)
  }

  val timerStatusSchema: Schema[TimerStatus] = Schema.record { field =>
    (
      field("timerStartTime", _.timerStartTime)(Schema[Long]),
      field.opt("pausedTime", _.pausedTime)(Schema[Long]),
      field("levels", _.levels)(timerLevelSchema.asList)
    ).mapN(TimerStatus.apply)
  }

  val phaseSchema: Schema[Phase] = Schema[String].imapErr {
    case "pre-flop" =>
      Right(PreFlop)
    case "flop" =>
      Right(Flop)
    case "turn" =>
      Right(Turn)
    case "river" =>
      Right(River)
    case "showdown" =>
      Right(Showdown)
    case otherPhase =>
      Left(Schema.ReadError(s"Invalid phase: $otherPhase"))
  }(Representations.phaseAsString)

  val gameDbSchema: Schema[GameDb] = Schema.record[GameDb] { field =>
    (
    field("gameCode", _.gameCode)(Schema[String]),
    field("gameId", _.gameId)(Schema[String]),
    field("expiry", _.expiry)(Schema[Long]),
    field("gameName", _.gameName)(Schema[String]),
    field("playerIds", _.playerIds)(Schema[String].asList),
    field("spectatorIds", _.spectatorIds)(Schema[String].asList),
    field("seed", _.seed)(Schema[Long]),
    field("phase", _.phase)(phaseSchema),
    field("smallBlind", _.smallBlind)(Schema[Int]),
    field.opt("inTurn", _.inTurn)(Schema[String]),
    field("button", _.button)(Schema[Int]),
    field("started", _.started)(Schema[Boolean]),
    field("startTime", _.startTime)(Schema[Long]),
    field("trackStacks", _.trackStacks)(Schema[Boolean]),
    field.opt("timer", _.timer)(timerStatusSchema),
    ).mapN(GameDb.apply)
  }

  val playerDbSchema: Schema[PlayerDb] = Schema.record[PlayerDb] { field =>
    (
      field("gameId", _.gameId)(Schema[String]),
      field("playerId", _.playerId)(Schema[String]),
      field("expiry", _.expiry)(Schema[Long]),
      field("playerAddress", _.playerAddress)(Schema[String]),
      field("playerKey", _.playerKey)(Schema[String]),
      field("screenName", _.screenName)(Schema[String]),
      field("stack", _.stack)(Schema[Int]),
      field("pot", _.pot)(Schema[Int]),
      field("bet", _.bet)(Schema[Int]),
      field("checked", _.checked)(Schema[Boolean]),
      field("folded", _.folded)(Schema[Boolean]),
      field("busted", _.busted)(Schema[Boolean]),
      field.opt("hole", _.hole)(holeSchema),
      field("holeVisible", _.holeVisible)(Schema[Boolean]),
      field("isHost", _.isHost)(Schema[Boolean]),
      field("isAdmin", _.isAdmin)(Schema[Boolean]),
      field("blind", _.blind)(Schema[Int]),
      field("isSpectator", _.isSpectator)(Schema[Boolean])
      ).mapN(PlayerDb.apply)
  }

  val gameEventDbSchema: Schema[GameEventDb] = Schema.oneOf[GameEventDb] { alt =>
    val schemaGS = Schema.record[GS] { field =>
      field("ps", _.ps)(Schema[String].asList)
        .map(GS.apply)
    }.tag("GS")
    val schemaNR = Schema.record[NR] { field =>
      (
        field("s", _.s)(Schema[Long]),
        field("b", _.b)(Schema[Int]),
        field.opt("sb", _.sb)(Schema[Int]),
        field.opt("sp", _.sp)(Schema[String]),
        field("bp", _.bp)(Schema[String]),
        field("ps", _.ps)(Schema[Int].asList)
      ).mapN(NR.apply)
    }.tag("NR")
    val schemaAR = Schema.record[AR] { field =>
      field.pure(AR())
    }.tag("AR")
    val schemaNP = Schema.record[NP] { field =>
      field("p", _.p)(Schema[String])
        .map(NP.apply)
    }.tag("NP")
    val schemaC = Schema.record[C] { field =>
      field("p", _.p)(Schema[String])
        .map(C.apply)
    }.tag("C")
    val schemaB = Schema.record[B] { field =>
      (
        field("p", _.p)(Schema[String]),
        field("b", _.b)(Schema[Int])
      ).mapN(B.apply)
    }.tag("B")
    val schemaF = Schema.record[F] { field =>
      field("p", _.p)(Schema[String])
        .map(F.apply)
    }.tag("F")
    val schemaGE = Schema.record[GE] { field =>
      field("w", _.w)(Schema[String])
        .map(GE.apply)
    }.tag("GE")

    alt(schemaGS) |+| alt(schemaNR) |+| alt(schemaAR) |+| alt(schemaNP) |+| alt(schemaC) |+| alt(schemaB) |+| alt(schemaF) |+| alt(schemaGE)
  }

  val eventRecordDbSchema: Schema[EventRecordDb] = Schema.record { field =>
    (
      field("gid", _.gid)(Schema[String]),
      field("ctd", _.ctd)(Schema[Long]),
      field("e", _.e)(gameEventDbSchema)
    ).mapN(EventRecordDb.apply)
  }
}
