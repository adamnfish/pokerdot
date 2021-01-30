package io.adamnfish.pokerdot.integration

import io.adamnfish.pokerdot.TestDates
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
              IO.unit
            }

            override def sendError(playerAddress: PlayerAddress, message: Failures): Attempt[Unit] = {
              IO.unit
            }
          },
          TestDates,
          testRng,
        )
        f(addressToContext, testDb)
      }
    }
  }
}
