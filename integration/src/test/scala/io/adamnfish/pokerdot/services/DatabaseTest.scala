package io.adamnfish.pokerdot.services

import io.adamnfish.pokerdot.TestHelpers
import io.adamnfish.pokerdot.integration.IntegrationComponents
import io.adamnfish.pokerdot.models.{AR, B, C, EP, F, GS, GameEventDb, GameId, GameLogEntryDb, NR}
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
            .distinctBy(_.ctd)
            .sortBy(_.ctd)
          val result = for {
            _ <- IO.foreach_(events)(db.writeGameEvent)
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
    "returns events back to the previous round break (without the round break)" in withDb { db =>
      val eventsGen = Gen.containerOf[List, GameEventDb](Arbitrary.arbitrary[GameEventDb])
      val phaseEndEventGen = Gen.oneOf(
        Arbitrary.arbitrary[NR],
        Arbitrary.arbitrary[EP],
        Arbitrary.arbitrary[AR],
      )
      val phaseEventsGen = Gen.containerOf[List, GameEventDb](Gen.oneOf(
        Arbitrary.arbitrary[C],
        Arbitrary.arbitrary[B],
        Arbitrary.arbitrary[F],
      ))

      forAll(eventsGen, phaseEndEventGen, phaseEventsGen) { (events, phaseEndEvent, phaseEvents) =>
        val gid = UUID.randomUUID().toString
        val allEvents = (events ::: List(phaseEndEvent) ::: phaseEvents)
          .zipWithIndex
          .map { case (ge, i) => GameLogEntryDb(gid, i, ge) }
        val result = for {
          _ <- IO.foreach_(allEvents)(db.writeGameEvent)
          logs <- db.getPhaseGameLog(GameId(gid))
        } yield logs
        result.value().map(_.e) shouldEqual phaseEvents.reverse
      }
    }
  }
}
