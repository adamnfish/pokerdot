package io.adamnfish.pokerdot.integration

import cats.effect.IO
import cats.effect.kernel.Resource
import io.adamnfish.pokerdot.{TestRng, TestTime}
import io.adamnfish.pokerdot.models.Serialisation.RequestEncoders.encodeRequest
import io.adamnfish.pokerdot.models.*
import io.adamnfish.pokerdot.persistence.DynamoDbDatabase
import io.adamnfish.pokerdot.services.{Database, Messaging, Rng}
import org.scanamo.LocalDynamoDB
import org.scanamo.LocalDynamoDB.deleteTable
import software.amazon.awssdk.auth.credentials.{AwsBasicCredentials, StaticCredentialsProvider}
import software.amazon.awssdk.regions.Region
import software.amazon.awssdk.services.dynamodb.{DynamoDbAsyncClient, DynamoDbClient}
import software.amazon.awssdk.services.dynamodb.model.{AttributeDefinition, CreateTableRequest, DeleteTableRequest, KeySchemaElement, KeyType, ScalarAttributeType}
import software.amazon.awssdk.services.dynamodb.model.ScalarAttributeType.*

import java.net.URI
import java.util.UUID.randomUUID
import java.util.function
import scala.util.Random


trait IntegrationComponents {
  private val client = DynamoDbAsyncClient.builder()
    .endpointOverride(URI.create("http://localhost:8042"))
    .region(Region.US_EAST_1) // not used for local dynamodb, but required
    .credentialsProvider(StaticCredentialsProvider.create(
      AwsBasicCredentials.create("dummykey", "dummysecret")))
    .build();

  def appContextRes: Resource[IO, (PlayerAddress => AppContext[IO], Database[IO])] =
    for {
      randomSuffix <- IO(randomUUID().toString).toResource
      gameTableName = s"games-$randomSuffix"
      playerTableName = s"players-$randomSuffix"
      connectionTableName = s"connections-$randomSuffix"
      testDb = new DynamoDbDatabase[IO](client, gameTableName, playerTableName, connectionTableName)
      testRng = new TestRng[IO]
      _ <- Resource.make(
        IO {
          val response = LocalDynamoDB.createTable(client)(gameTableName)("gameCode" -> S, "gameId" -> S)
          response.tableDescription().tableName()
        }
      )(tableName => IO(deleteTable(client)(tableName)))
      _ <- Resource.make(
        IO {
          val response = LocalDynamoDB.createTable(client)(playerTableName)("gameId" -> S, "playerId" -> S)
          response.tableDescription().tableName()
        }
      )(tableName => IO(deleteTable(client)(tableName)))
      _ <- Resource.make(
        IO {
          val response = LocalDynamoDB.createTable(client)(connectionTableName)("gameId" -> S, "address" -> S)
          response.tableDescription().tableName()
        }
      )(tableName => IO(deleteTable(client)(tableName)))
      addressToContext = { (playerAddress: PlayerAddress) =>
        AppContext(
          playerAddress,
          TraceId("trace-id"),
          testDb,
          // TODO: keep track of sent messages so we can perform assertions on that as well
          new Messaging[IO] {
            override def sendMessage(playerAddress: PlayerAddress, message: Message): IO[Unit] = {
              IO.unit
            }

            override def sendError(playerAddress: PlayerAddress, message: Failures): IO[Unit] = {
              IO.unit
            }
          },
          new TestTime[IO],
          testRng,
        )
      }
    } yield (addressToContext, testDb)
}
object IntegrationComponents {
  /**
   * Runs the effect after players are read, to simulate a concurrent request between a read and a write.
   */
  def afterGetPlayers(db: Database[IO])(effect: List[PlayerDb] => IO[Unit]): Database[IO] =
    new Database[IO] {
      override def getGame(gameId: GameId): IO[Option[GameDb]] = db.getGame(gameId)
      override def lookupGame(gameCode: String): IO[Option[GameDb]] = db.lookupGame(gameCode)
      override def searchGameCode(gameCode: String): IO[List[GameDb]] = db.searchGameCode(gameCode)
      override def getPlayers(gameId: GameId): IO[List[PlayerDb]] = db.getPlayers(gameId).flatTap(effect)
      override def writeGame(gameDb: GameDb): IO[Unit] = db.writeGame(gameDb)
      override def writePlayer(playerDb: PlayerDb): IO[Unit] = db.writePlayer(playerDb)
      override def putConnection(connection: ConnectionDb): IO[Unit] = db.putConnection(connection)
      override def getConnections(gameId: GameId): IO[List[ConnectionDb]] = db.getConnections(gameId)
    }

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
