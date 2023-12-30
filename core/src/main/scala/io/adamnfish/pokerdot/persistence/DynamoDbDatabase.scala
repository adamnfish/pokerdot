package io.adamnfish.pokerdot.persistence

import software.amazon.awssdk.services.dynamodb.DynamoDbClient
import io.adamnfish.dynamodecs.ItemCodec
import io.adamnfish.pokerdot.logic.Games
import io.adamnfish.pokerdot.logic.Utils.{EitherUtils, RichList}
import io.adamnfish.pokerdot.models.{Attempt, Failure, Failures, GameDb, GameId, PlayerDb, Serialisation}
import io.adamnfish.pokerdot.services.Database
import software.amazon.awssdk.services.dynamodb.model.{AttributeValue, DynamoDbRequest, DynamoDbResponse, GetItemRequest, GetItemResponse, PutItemRequest, PutItemResponse, QueryRequest, QueryResponse}
import zio.ZIO

import scala.jdk.CollectionConverters.*


class DynamoDbDatabase(client: DynamoDbClient, gameTableName: String, playerTableName: String) extends Database {

//  private val games = Table[GameDb](gameTableName)
//  private val players = Table[PlayerDb](playerTableName)

  // TODO: consider whether this should just derive a gameCode and call lookup
  override def getGame(gameId: GameId): Attempt[Option[GameDb]] = {
    val gameCode = Games.gameCode(gameId)
    for {
      response <- sendRequest[GetItemRequest, GetItemResponse](
        client.getItem,
        GetItemRequest.builder()
          .tableName(gameTableName)
          .key(Map("gameCode" -> AttributeValue.fromS(gameCode), "gameId" -> AttributeValue.fromS(gameId.gid)).asJava)
          .build()
      )
      // TODO: handle not found case
    } yield Some(Serialisation.db.gameDbCodec.decode(response.item()))
  }

  override def lookupGame(gameCode: String): Attempt[Option[GameDb]] = {
    for {
      response <- sendRequest[QueryRequest, QueryResponse](
        client.query,
        QueryRequest.builder()
          .tableName(gameTableName)
          .keyConditionExpression("gameCode = :gameCode")
          .expressionAttributeValues(Map(":gameCode" -> AttributeValue.fromS(gameCode)).asJava)
          .build()
      )
      maybeResult <- response.items().asScala.toList.map(Serialisation.db.gameDbCodec.decode(_)) match {
        case Nil =>
          ZIO.succeed(None)
        case result :: Nil =>
          ZIO.succeed(Some(result))
        case _ =>
          Failure(
            s"Multiple games found for code `$gameCode`",
            "couldn't find a game for that code",
          ).asIO
      }
    } yield maybeResult
  }


  override def searchGameCode(gameCode: String): Attempt[List[GameDb]] = {
    for {
      response <- sendRequest[QueryRequest, QueryResponse](
        client.query,
        QueryRequest.builder()
          .tableName(gameTableName)
          .keyConditionExpression("gameCode = :gameCode")
          .expressionAttributeValues(Map(":gameCode" -> AttributeValue.fromS(gameCode)).asJava)
          .build()
      )
    } yield response.items().asScala.toList.map(Serialisation.db.gameDbCodec.decode(_))
  }

  override def getPlayers(gameId: GameId): Attempt[List[PlayerDb]] = {
    for {
      response <- sendRequest[QueryRequest, QueryResponse](
        client.query,
        QueryRequest.builder()
          .tableName(playerTableName)
          .keyConditionExpression("gameId = :gameId")
          .expressionAttributeValues(Map(":gameId" -> AttributeValue.fromS(gameId.gid)).asJava)
          .build()
      )
    } yield response.items().asScala.toList.map(Serialisation.db.playerDbCodec.decode(_))
  }

  override def writeGame(gameDB: GameDb): Attempt[Unit] = {
    val item = Serialisation.db.gameDbCodec.encode(gameDB)
    for {
      result <- sendRequest[PutItemRequest, PutItemResponse](
        (r: PutItemRequest) => client.putItem(r),
        PutItemRequest.builder()
          .tableName(gameTableName)
          .item(item.asJava)
          .build()
      )
    } yield ()
  }

  override def writePlayer(playerDB: PlayerDb): Attempt[Unit] = {
    val item = Serialisation.db.playerDbCodec.encode(playerDB)
    for {
      result <- sendRequest[PutItemRequest, PutItemResponse](
        (r: PutItemRequest) => client.putItem(r),
        PutItemRequest.builder()
          .tableName(playerTableName)
          .item(item.asJava)
          .build()
      )
    } yield ()
  }

  private def sendRequest[Req <: DynamoDbRequest, Res <: DynamoDbResponse](method: Req => Res, req: Req): Attempt[Res] = {
    ZIO.attemptBlocking {
      method(req)
    }.mapError { err =>
      Failures("Uncaught DB error", "I had a problem saving the game", None, Some(err))
    }
  }
}
