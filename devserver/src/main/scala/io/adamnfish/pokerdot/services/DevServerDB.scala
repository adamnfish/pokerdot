package io.adamnfish.pokerdot.services

import org.scanamo.LocalDynamoDB
import software.amazon.awssdk.services.dynamodb.DynamoDbAsyncClient
import software.amazon.awssdk.services.dynamodb.model.ScalarAttributeType.*
import cats.effect.IO

object DevServerDB {
  def createGamesTable(client: DynamoDbAsyncClient): IO[Unit] = {
    IO.blocking {
      LocalDynamoDB.createTable(client)("games")(
        "gameCode" -> S,
        "gameId" -> S
      )
    }
  }

  def createPlayersTable(client: DynamoDbAsyncClient): IO[Unit] = {
    IO.blocking {
      LocalDynamoDB.createTable(client)("players")(
        "gameId" -> S,
        "playerId" -> S
      )
    }
  }
}
