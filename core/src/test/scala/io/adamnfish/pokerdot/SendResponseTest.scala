package io.adamnfish.pokerdot

import io.adamnfish.pokerdot.models.*
import io.adamnfish.pokerdot.services.{Database, Gone, Messaging, SendResult, Sent}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable


class SendResponseTest extends AnyFreeSpec with Matchers {
  type Result[A] = Either[Throwable, A]

  class RecordingMessaging(failing: Set[PlayerAddress], gone: Set[PlayerAddress] = Set.empty) extends Messaging[Result] {
    val sent = mutable.ListBuffer.empty[PlayerAddress]

    override def sendMessage(playerAddress: PlayerAddress, message: Message): Result[SendResult] = {
      if (failing.contains(playerAddress))
        Left(Failures("send failed", "send failed", internal = true))
      else if (gone.contains(playerAddress))
        Right(Gone)
      else {
        sent += playerAddress
        Right(Sent)
      }
    }

    override def sendError(playerAddress: PlayerAddress, message: Failures): Result[SendResult] = Right(Sent)
  }

  class RecordingDatabase(removalFails: Boolean = false) extends Database[Result] {
    val removed = mutable.ListBuffer.empty[(GameId, PlayerAddress)]

    override def removeConnection(gameId: GameId, address: PlayerAddress): Result[Unit] = {
      if (removalFails) Left(Failures("removal failed", "error fetching saved data"))
      else Right(removed += (gameId -> address))
    }

    override def getGame(gameId: GameId): Result[Option[GameDb]] = ???
    override def lookupGame(gameCode: String): Result[Option[GameDb]] = ???
    override def searchGameCode(gameCode: String): Result[List[GameDb]] = ???
    override def getPlayers(gameId: GameId): Result[List[PlayerDb]] = ???
    override def writeGame(gameDB: GameDb): Result[Unit] = ???
    override def writePlayer(playerDB: PlayerDb): Result[Unit] = ???
    override def putConnection(connection: ConnectionDb): Result[Unit] = ???
    override def getConnections(gameId: GameId): Result[List[ConnectionDb]] = ???
  }

  val gameId = GameId("game-id")
  val addresses = List("a", "b", "c").map(PlayerAddress(_))
  val response = Response[Message](addresses.map(_ -> Status("ok")).toMap, Map.empty, Some(gameId))

  "sends to every address" in {
    val messaging = new RecordingMessaging(Set.empty)
    PokerDot.sendResponse(response, messaging, new RecordingDatabase) shouldEqual Right(())
    messaging.sent.toSet shouldEqual addresses.toSet
  }

  "still sends to the other addresses when one fails" in {
    val messaging = new RecordingMessaging(Set(addresses.head))
    PokerDot.sendResponse(response, messaging, new RecordingDatabase)
    messaging.sent.toSet shouldEqual addresses.tail.toSet
  }

  "returns the send failures" in {
    val messaging = new RecordingMessaging(Set(addresses.head, addresses.last))
    val result = PokerDot.sendResponse(response, messaging, new RecordingDatabase)
    result.left.toOption.collect { case f: Failures => f.failures.length } shouldEqual Some(2)
  }

  "send failures stay internal" in {
    val messaging = new RecordingMessaging(Set(addresses.head))
    val result = PokerDot.sendResponse(response, messaging, new RecordingDatabase)
    result.left.toOption.collect { case f: Failures => f.externalFailures } shouldEqual Some(Nil)
  }

  "for a gone connection" - {
    "removes the connection" in {
      val db = new RecordingDatabase
      PokerDot.sendResponse(response, new RecordingMessaging(Set.empty, Set(addresses.head)), db)
      db.removed.toList shouldEqual List(gameId -> addresses.head)
    }

    "does not count it as a failure" in {
      val result = PokerDot.sendResponse(response, new RecordingMessaging(Set.empty, Set(addresses.head)), new RecordingDatabase)
      result shouldEqual Right(())
    }

    "still sends to the other addresses" in {
      val messaging = new RecordingMessaging(Set.empty, Set(addresses.head))
      PokerDot.sendResponse(response, messaging, new RecordingDatabase)
      messaging.sent.toSet shouldEqual addresses.tail.toSet
    }

    "keeps a failure to remove the connection internal" in {
      val result = PokerDot.sendResponse(response, new RecordingMessaging(Set.empty, Set(addresses.head)), new RecordingDatabase(removalFails = true))
      result.left.toOption.collect { case f: Failures => (f.failures.length, f.externalFailures) } shouldEqual Some((1, Nil))
    }

    "does nothing if the response isn't for a game" in {
      val db = new RecordingDatabase
      PokerDot.sendResponse(response.copy(gameId = None), new RecordingMessaging(Set.empty, Set(addresses.head)), db)
      db.removed shouldBe empty
    }
  }
}
