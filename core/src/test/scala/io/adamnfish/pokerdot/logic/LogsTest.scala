package io.adamnfish.pokerdot.logic

import io.adamnfish.pokerdot.TestHelpers
import io.adamnfish.pokerdot.logic.Logs.tryToGetAllPhaseEvents
import io.adamnfish.pokerdot.models.{B, C, GS, GameLogEntryDb, NP, NR}
import org.scalatest.freespec.AnyFreeSpec


class LogsTest extends AnyFreeSpec with TestHelpers {
  "tryToGetAllPhaseEvents" - {
    "if a phase has just started" - {
      val phaseEvents = List(
        GameLogEntryDb("gid", 1000, NP("p"))
      )
      val previousEvents = List(
        GameLogEntryDb("gid", 1000, NR(1, 0, Some(5), Some("p2id"), "p3id", List(1000, 1000, 1000))),
        GameLogEntryDb("gid", 1000, GS(List("p1id", "p2id", "p3id"))),
      )

      "returns the new phase event" in {
        val (result, _) = tryToGetAllPhaseEvents(phaseEvents ++ previousEvents)
        result shouldEqual phaseEvents
      }

      "returns finished as true" in {
        val (_, finished) = tryToGetAllPhaseEvents(phaseEvents ++ previousEvents)
        finished shouldEqual true
      }
    }

    "if a phase is in progress" - {
      val phaseEvents = List(
        GameLogEntryDb("gid", 1000, B("p2id", 10)),
        GameLogEntryDb("gid", 1000, C("p2id")),
        GameLogEntryDb("gid", 1000, NP("p"))
      )
      val previousEvents = List(
        GameLogEntryDb("gid", 1000, NR(1, 0, Some(5), Some("p2id"), "p3id", List(1000, 1000, 1000))),
        GameLogEntryDb("gid", 1000, GS(List("p1id", "p2id", "p3id"))),
      )

      "returns the phase events so far" in {
        val (result, _) = tryToGetAllPhaseEvents(phaseEvents ++ previousEvents)
        result shouldEqual phaseEvents
      }

      "returns finished as true" in {
        val (_, finished) = tryToGetAllPhaseEvents(phaseEvents ++ previousEvents)
        finished shouldEqual true
      }
    }

    "if there is not a phase" - {
      "returns nothing if there are no events" in {
        val (result, _) = tryToGetAllPhaseEvents(Nil)
        result shouldEqual Nil
      }

      "finished is true if there are no events" in {
        val (_, finished) = tryToGetAllPhaseEvents(Nil)
        finished shouldEqual true
      }

      "returns nothing if the game is just getting started" in {
        val events = List(
          GameLogEntryDb("gid", 1000, NR(1, 0, Some(5), Some("p2id"), "p3id", List(1000, 1000, 1000))),
          GameLogEntryDb("gid", 1000, GS(List("p1id", "p2id", "p3id"))),
        )
        val (result, _) = tryToGetAllPhaseEvents(events)
        result shouldEqual Nil
      }
    }

    "if the results don't go far enough back" - {
      val events = List.fill(10)(List(
        GameLogEntryDb("gid", 1000, B("p1id", 5)),
        GameLogEntryDb("gid", 1000, B("p2id", 5)),
        GameLogEntryDb("gid", 1000, B("p3id", 5)),
      )).flatten

      "return everything we have" in {
        val (result, _) = tryToGetAllPhaseEvents(events)
        result shouldEqual events
      }

      "return false to indicate more events are required" in {
        val (_, finished) = tryToGetAllPhaseEvents(events)
        finished shouldEqual false
      }
    }
  }
}
