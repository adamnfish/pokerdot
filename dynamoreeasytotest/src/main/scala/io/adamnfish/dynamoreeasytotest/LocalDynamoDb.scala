package io.adamnfish.dynamoreeasytotest

import software.amazon.awssdk.services.dynamodb.DynamoDbClient
import software.amazon.awssdk.services.dynamodb.model.{AttributeDefinition, CreateTableRequest, DeleteTableRequest, KeySchemaElement, KeyType, ProvisionedThroughput, ScalarAttributeType}

import scala.jdk.CollectionConverters.*


object LocalDynamoDb {
  def withTable[A](client: DynamoDbClient, tableName: String, attributeDefinitions: (String, ScalarAttributeType)*)(body: => A): A = {
    createDbTable(client, tableName, attributeDefinitions: _*)
    val result =
      try body
      finally deleteTable(client, tableName)
    result
  }

  private def attributeDefinitions(attributes: Seq[(String, ScalarAttributeType)]): List[AttributeDefinition] =
    attributes
      .map { case (symbol, attributeType) =>
        AttributeDefinition.builder.attributeName(symbol).attributeType(attributeType).build
      }
      .toList

  private def keySchema(attributes: List[(String, ScalarAttributeType)]): List[KeySchemaElement] = {
    attributes match {
      case hashKeyWithType :: rangeKeyWithType =>
        val keySchemas = hashKeyWithType._1 -> KeyType.HASH :: rangeKeyWithType.map(_._1 -> KeyType.RANGE)
        keySchemas.map { case (symbol, keyType) =>
          KeySchemaElement.builder.attributeName(symbol).keyType(keyType).build
        }
      case Nil =>
        throw new IllegalArgumentException("Must provide at least one attribute")
    }
  }

  def createDbTable(client: DynamoDbClient, tableName: String, attributes: (String, ScalarAttributeType)*) =
    client
      .createTable(
        CreateTableRequest.builder
          .attributeDefinitions(attributeDefinitions(attributes).asJava)
          .tableName(tableName)
          .keySchema(keySchema(attributes.toList).asJava)
          .provisionedThroughput(
            // not used locally, but required
            ProvisionedThroughput.builder.readCapacityUnits(1L).writeCapacityUnits(1L).build
          )
          .build
      )

  private def deleteTable(client: DynamoDbClient, tableName: String) =
    client.deleteTable(
      DeleteTableRequest.builder
        .tableName(tableName)
        .build
    )
}
