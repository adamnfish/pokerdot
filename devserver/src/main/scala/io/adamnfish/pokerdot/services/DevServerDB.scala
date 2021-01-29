package io.adamnfish.pokerdot.services

import org.scanamo.LocalDynamoDB
import software.amazon.awssdk.services.dynamodb.DynamoDbClient
import software.amazon.awssdk.services.dynamodb.model.ScalarAttributeType._


object DevServerDB {
  def createGamesTable(client: DynamoDbClient): Unit = {
    LocalDynamoDB.createTable(client)("games")(
      "gameCode" -> S,
      "gameId" -> S,
    )
  }

  def createPlayersTable(client: DynamoDbClient): Unit = {
    LocalDynamoDB.createTable(client)("players")(
      "gameId" -> S,
      "playerId" -> S,
    )
  }
}
