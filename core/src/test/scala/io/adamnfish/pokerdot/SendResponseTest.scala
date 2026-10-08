package io.adamnfish.pokerdot

import io.adamnfish.pokerdot.models.*
import io.adamnfish.pokerdot.services.Messaging
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable


class SendResponseTest extends AnyFreeSpec with Matchers {
  type Result[A] = Either[Throwable, A]

  class RecordingMessaging(failing: Set[PlayerAddress]) extends Messaging[Result] {
    val sent = mutable.ListBuffer.empty[PlayerAddress]

    override def sendMessage(playerAddress: PlayerAddress, message: Message): Result[Unit] = {
      if (failing.contains(playerAddress))
        Left(Failures("send failed", "send failed", internal = true))
      else {
        sent += playerAddress
        Right(())
      }
    }

    override def sendError(playerAddress: PlayerAddress, message: Failures): Result[Unit] = Right(())
  }

  val addresses = List("a", "b", "c").map(PlayerAddress(_))
  val response = Response[Message](addresses.map(_ -> Status("ok")).toMap, Map.empty)

  "sends to every address" in {
    val messaging = new RecordingMessaging(Set.empty)
    PokerDot.sendResponse(response, messaging) shouldEqual Right(())
    messaging.sent.toSet shouldEqual addresses.toSet
  }

  "still sends to the other addresses when one fails" in {
    val messaging = new RecordingMessaging(Set(addresses.head))
    PokerDot.sendResponse(response, messaging)
    messaging.sent.toSet shouldEqual addresses.tail.toSet
  }

  "returns the send failures" in {
    val messaging = new RecordingMessaging(Set(addresses.head, addresses.last))
    val result = PokerDot.sendResponse(response, messaging)
    result.left.toOption.collect { case f: Failures => f.failures.length } shouldEqual Some(2)
  }

  "send failures stay internal" in {
    val messaging = new RecordingMessaging(Set(addresses.head))
    val result = PokerDot.sendResponse(response, messaging)
    result.left.toOption.collect { case f: Failures => f.externalFailures } shouldEqual Some(Nil)
  }
}
