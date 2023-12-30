package io.adamnfish.pokerdot.integration

import io.adamnfish.pokerdot.TestClock
import io.adamnfish.pokerdot.models.Serialisation.RequestEncoders.encodeRequest
import io.adamnfish.pokerdot.models._
import io.adamnfish.dynamoreeasytotest.LocalDynamoDb
import io.adamnfish.pokerdot.persistence.DynamoDbDatabase
import io.adamnfish.pokerdot.services.{Database, Messaging, Rng}
import software.amazon.awssdk.auth.credentials.{AwsBasicCredentials, StaticCredentialsProvider}
import software.amazon.awssdk.regions.Region
import software.amazon.awssdk.services.dynamodb.DynamoDbClient
import software.amazon.awssdk.services.dynamodb.model.{AttributeDefinition, CreateTableRequest, DeleteTableRequest, KeySchemaElement, KeyType, ScalarAttributeType}
import software.amazon.awssdk.services.dynamodb.model.ScalarAttributeType._
import zio.ZIO

import java.net.URI
import java.util.UUID.randomUUID
import java.util.function
import scala.util.Random


trait IntegrationComponents {
  private val client = DynamoDbClient.builder()
    .endpointOverride(URI.create("http://localhost:8042"))
    .region(Region.US_EAST_1) // not used for local dynamodb, but required
    .credentialsProvider(StaticCredentialsProvider.create(
      AwsBasicCredentials.create("dummykey", "dummysecret")))
    .build();

  def withAppContext(f: (PlayerAddress => AppContext, Database) => Any /* Assertion */): Any /* Assertion */ = {
    val randomSuffix = randomUUID().toString
    val gameTableName = s"games-$randomSuffix"
    val playerTableName = s"players-$randomSuffix"
    val testDb = new DynamoDbDatabase(client, gameTableName, playerTableName)
    val testRng = new Rng {
      override def randomState(): Long = 0
      override def nextState(state: Long): Long = new Random(state).nextLong()
    }

    LocalDynamoDb.withTable(client, gameTableName, "gameCode" -> S, "gameId" -> S) {
      LocalDynamoDb.withTable(client, playerTableName, "gameId" -> S, "playerId" -> S) {
        val addressToContext = { (playerAddress: PlayerAddress) =>
          AppContext(
            playerAddress,
            TraceId("trace-id"),
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
        }
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
