package io.adamnfish.pokerdot.integration

import io.adamnfish.pokerdot.TestClock
import io.adamnfish.pokerdot.models.Serialisation.RequestEncoders.encodeRequest
import io.adamnfish.pokerdot.models._
import io.adamnfish.pokerdot.persistence.DynamoDbDatabase
import io.adamnfish.pokerdot.services.{Database, Messaging, Rng}
import org.scanamo.LocalDynamoDB
import software.amazon.awssdk.services.dynamodb.model.ScalarAttributeType._
import zio.IO

import java.util.UUID.randomUUID
import scala.util.Random


trait IntegrationComponents {
  private val client = LocalDynamoDB.syncClient()

  def withAppContext(f: (PlayerAddress => AppContext, Database) => Any /* Assertion */): Any /* Assertion */ = {
    val testRng = new Rng {
      override def randomState(): Long = 0
      override def nextState(state: Long): Long = new Random(state).nextLong()
    }
    val testMessaging = new Messaging {
      override def sendMessage(playerAddress: PlayerAddress, message: Message): Attempt[Unit] = IO.unit
      override def sendError(playerAddress: PlayerAddress, message: Failures): Attempt[Unit] = IO.unit
    }

    withDb { testDb =>
      val addressToContext = AppContext(
        _,
        testDb,
        testMessaging,
        TestClock,
        testRng,
      )
      f(addressToContext, testDb)
    }
  }

  def withDb(f: Database => Any /* Assertion */): Any /* Assertion */ = {
    val randomSuffix = randomUUID().toString
    val gameTableName = s"games-$randomSuffix"
    val playerTableName = s"players-$randomSuffix"
    val gameLogTableName = s"game-logs-$randomSuffix"

    val testDb = new DynamoDbDatabase(client, gameTableName, playerTableName, gameLogTableName)

    LocalDynamoDB.withTable(client)(gameTableName)("gameCode" -> S, "gameId" -> S) {
      LocalDynamoDB.withTable(client)(playerTableName)("gameId" -> S, "playerId" -> S) {
        LocalDynamoDB.withTable(client)(gameLogTableName)("gid" -> S, "ctd" -> N) {
          f(testDb)
        }
      }
    }
  }
}
object IntegrationComponents {
  def betRequest(betAmount: Int, welcome: Welcome): String = {
    val request = Bet(welcome.gameId, welcome.playerKey, welcome.playerId, betAmount)
    encodeRequest(request).noSpaces
  }

  def checkRequest(welcome: Welcome): String = {
    val request = Check(welcome.gameId, welcome.playerKey, welcome.playerId)
    encodeRequest(request).noSpaces
  }

  def foldRequest(welcome: Welcome): String = {
    val request = Fold(welcome.gameId, welcome.playerKey, welcome.playerId)
    encodeRequest(request).noSpaces
  }

  def advancePhaseRequest(welcome: Welcome): String = {
    val request = AdvancePhase(welcome.gameId, welcome.playerKey, welcome.playerId)
    encodeRequest(request).noSpaces
  }

  def abandonRoundRequest(welcome: Welcome): String = {
    val request = AbandonRound(welcome.gameId, welcome.playerKey, welcome.playerId)
    encodeRequest(request).noSpaces
  }
}
