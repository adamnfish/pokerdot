package io.adamnfish.pokerdot.integration

import io.adamnfish.pokerdot.TestClock
import io.adamnfish.pokerdot.models.Serialisation.RequestEncoders.encodeRequest
import io.adamnfish.pokerdot.models._
import io.adamnfish.pokerdot.persistence.DynamoDbDatabase
import io.adamnfish.pokerdot.services.{Database, Messaging, Rng}
import org.scanamo.LocalDynamoDB
import software.amazon.awssdk.services.dynamodb.model.ScalarAttributeType._
import zio.ZIO

import java.util.UUID.randomUUID
import scala.util.Random


trait IntegrationComponents {
  private val client = LocalDynamoDB.syncClient()

  def withAppContext(f: (PlayerAddress => AppContext, Database) => Any /* Assertion */): Any /* Assertion */ = {
    val randomSuffix = randomUUID().toString
    val gameTableName = s"games-$randomSuffix"
    val playerTableName = s"players-$randomSuffix"
    val testDb = new DynamoDbDatabase(client, gameTableName, playerTableName)
    val testRng = new Rng {
      override def randomState(): Long = 0
      override def nextState(state: Long): Long = new Random(state).nextLong()
    }

    LocalDynamoDB.withTable(client)(gameTableName)("gameCode" -> S, "gameId" -> S) {
      LocalDynamoDB.withTable(client)(playerTableName)("gameId" -> S, "playerId" -> S) {
        val addressToContext = AppContext(
          _,
          testDb,
          new Messaging {
            override def sendMessage(playerAddress: PlayerAddress, message: Message): Attempt[Unit] = {
              ZIO.unit
            }

            override def sendError(playerAddress: PlayerAddress, message: Failures): Attempt[Unit] = {
              ZIO.unit
            }
          },
          TestClock,
          testRng,
        )
        f(addressToContext, testDb)
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
}
