package io.adamnfish.pokerdot.services

import io.adamnfish.pokerdot.TestHelpers
import io.adamnfish.pokerdot.integration.IntegrationComponents
import io.adamnfish.pokerdot.models.{AR, B, C, F, GS, GameEventDb, GameId, GameLogEntryDb, NP, NR}
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.ScalacheckShapeless.derivedArbitrary
import org.scalatest.freespec.AnyFreeSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import zio.IO

import java.util.UUID

class DatabaseTest extends AnyFreeSpec with TestHelpers with IntegrationComponents with ScalaCheckDrivenPropertyChecks {
  implicitly[Arbitrary[GameLogEntryDb]]

  "game log" - {
    "getFullGameLog" - {
      "returns all events for the game, newest first" in withDb { db =>
        forAll { genEvents: List[GameLogEntryDb] =>
          val gid = UUID.randomUUID().toString
          val events = genEvents
            .map(_.copy(gid = gid))
            .map {
              // empty optional strings don't get round-tripped properly
              // but this doesn't matter because these IDs are never empty, so we patch it here
              case gle @ GameLogEntryDb(_, _, nr @ NR(_, _, _, Some(""), _, _)) =>
                gle.copy(e = nr.copy(sp = None))
              case gle =>
                gle
            }
            .distinctBy(_.ctd)
            .sortBy(_.ctd)
          val result = for {
            _ <- db.writeGameEvents(events.toSet)
            logs <- db.getFullGameLog(GameId(gid))
          } yield logs
          result.value() shouldEqual events.reverse
        }
      }

      "returns nothing if there are not yet any events" in withDb { db =>
        val result = db.getFullGameLog(GameId("does not exist")).value()
        result shouldBe empty
      }
    }
  }

  "getPhaseGameLog" - {
    "returns events back to the start of the phase (inclusive)" in withDb { db =>
      val eventsGen = Gen.containerOf[List, GameEventDb](Arbitrary.arbitrary[GameEventDb])
      val newPhaseEventGen = Arbitrary.arbitrary[NP]
      val phaseEventsGen = Gen.containerOf[List, GameEventDb](Gen.oneOf(
        Arbitrary.arbitrary[C],
        Arbitrary.arbitrary[B],
        Arbitrary.arbitrary[F],
      ))

      forAll(eventsGen, newPhaseEventGen, phaseEventsGen) { (events, newPhaseEvent, phaseEvents) =>
        val gid = UUID.randomUUID().toString
        val allEvents = (events ::: List(newPhaseEvent) ::: phaseEvents)
          .zipWithIndex
          .map { case (ge, i) => GameLogEntryDb(gid, i, ge) }
        val result = for {
          _ <- IO.foreach_(allEvents)(db.writeGameEvent)
          logs <- db.getPhaseGameLog(GameId(gid))
        } yield logs
        result.value().map(_.e) shouldEqual (phaseEvents.reverse :+ newPhaseEvent)
      }
    }
  }
}
