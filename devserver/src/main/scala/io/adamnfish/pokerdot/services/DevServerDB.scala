package io.adamnfish.pokerdot.services

import io.adamnfish.dynamoreeasytotest.LocalDynamoDb
import software.amazon.awssdk.services.dynamodb.DynamoDbClient
import software.amazon.awssdk.services.dynamodb.model.ScalarAttributeType.*


object DevServerDB {
  def createGamesTable(client: DynamoDbClient): Unit = {
    LocalDynamoDb.createDbTable(client, "games",
      "gameCode" -> S,
      "gameId" -> S,
    )
  }

  def createPlayersTable(client: DynamoDbClient): Unit = {
    LocalDynamoDb.createDbTable(client, "players",
      "gameId" -> S,
      "playerId" -> S,
    )
  }
}
