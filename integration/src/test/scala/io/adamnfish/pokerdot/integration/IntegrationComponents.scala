package io.adamnfish.pokerdot.integration

import java.util.UUID.randomUUID

import com.amazonaws.services.dynamodbv2.model.ScalarAttributeType._
import io.adamnfish.pokerdot.Messaging
import io.adamnfish.pokerdot.models._
import io.adamnfish.pokerdot.persistence.{Database, DynamoDb}
import io.circe.Encoder
import io.circe.generic.semiauto.deriveEncoder
import org.scanamo.LocalDynamoDB
import zio.IO


trait IntegrationComponents {
  private val client = LocalDynamoDB.client()

  def withAppContext(f: (PlayerAddress => AppContext, Database) => Any /* Assertion */): Any /* Assertion */ = {
    val randomSuffix = randomUUID().toString
    val gameTableName = s"games-$randomSuffix"
    val playerTableName = s"players-$randomSuffix"
    val testDb = new DynamoDb(client, gameTableName, playerTableName)

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
          }
        )
        f(addressToContext, testDb)
      }
    }
  }
}
