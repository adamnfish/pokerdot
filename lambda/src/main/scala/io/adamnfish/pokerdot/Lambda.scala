package io.adamnfish.pokerdot


import java.util.concurrent.Executors
import software.amazon.awssdk.regions.Region
import com.amazonaws.services.lambda.runtime.{Context => AwsContext}
import com.amazonaws.services.lambda.runtime.events.{APIGatewayV2WebSocketEvent, APIGatewayV2WebSocketResponse}
import io.adamnfish.pokerdot.models.{AppContext, PlayerAddress}
import software.amazon.awssdk.services.apigatewaymanagementapi.ApiGatewayManagementApiClient

import java.net.{HttpURLConnection, URI}
import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.util.Properties
import io.adamnfish.pokerdot.persistence.DynamoDb
import io.adamnfish.pokerdot.services.{Dates, RandomRng}
import software.amazon.awssdk.auth.credentials.EnvironmentVariableCredentialsProvider
import software.amazon.awssdk.services.dynamodb.DynamoDbClient
import zio.IO

class Lambda {
  implicit private val ec: ExecutionContext = ExecutionContext.fromExecutor(Executors.newCachedThreadPool)

  private val db = {
    val maybeDb = for {
      regionStr <- Properties.envOrNone("REGION")
        .toRight("region not configured")
      region = Region.of(regionStr)
    } yield {
      val dynamoDbClient = DynamoDbClient.builder()
        .credentialsProvider(EnvironmentVariableCredentialsProvider.create())
        .region(region)
        .build()
      val maybeDb = for {
        gamesTableName <- Properties.envOrNone("GAMES_TABLE")
          .toRight("games table name not configured")
        playersTableName <- Properties.envOrNone("PLAYERS_TABLE")
          .toRight("players table name not configured")
      } yield new io.adamnfish.pokerdot.persistence.DynamoDb(dynamoDbClient, gamesTableName, playersTableName)
      maybeDb.fold(
        { errMsg =>
          throw new RuntimeException(errMsg)
        },
        identity
      )
    }
    maybeDb.fold(
      { errMsg =>
        throw new RuntimeException(errMsg)
      },
      identity
    )
  }
  val apiGatewayMessagingClient = {
    val maybeApiGatewayClient = for {
      apiGatewayEndpointStr <- Properties.envOrNone("API_ORIGIN_LOCATION")
        .toRight("API Gateway endpoint name not configured")
      apiGatewayEndpointUri = new URI(s"https://$apiGatewayEndpointStr")
      regionStr <- Properties.envOrNone("REGION")
        .toRight("region not configured")
      region = Region.of(regionStr)
    } yield {
      ApiGatewayManagementApiClient.builder()
        .credentialsProvider(EnvironmentVariableCredentialsProvider.create())
        .region(region)
        .endpointOverride(apiGatewayEndpointUri)
        .build()
    }
    maybeApiGatewayClient.fold(
      { errMsg =>
        throw new RuntimeException(errMsg)
      },
      identity
    )
  }
  val dates = Dates
  val rng = new RandomRng

  def handleRequest(event: APIGatewayV2WebSocketEvent, awsContext: AwsContext): APIGatewayV2WebSocketResponse = {
    val awsMessaging = new AwsMessaging(apiGatewayMessagingClient, awsContext.getLogger)
    // Debugging for now
    awsContext.getLogger.log(s"request body: ${event.getBody}")
    awsContext.getLogger.log(s"connection ID: ${event.getRequestContext.getConnectionId}")
    awsContext.getLogger.log(s"route: ${event.getRequestContext.getRouteKey}")

    event.getRequestContext.getRouteKey match {
      case "$connect" =>
      // ignore this for now
      case "$disconnect" =>
      // ignore this for now
      case "$default" =>
        val playerAddress = PlayerAddress(event.getRequestContext.getConnectionId)
        val appContext = AppContext(playerAddress, db, awsMessaging, dates, rng)

        zio.Runtime.default.unsafeRunSync(
          PokerDot.pokerdot(event.getBody, appContext)
        ).fold(
          { cause =>
            cause.failureOption match {
              case Some(fs) =>
                awsContext.getLogger.log(
                  s"Request failed: ${fs.logString}"
                )
              case None =>
                awsContext.getLogger.log(
                  s"Request failed with no failures"
                )
            }
          },
          { operation =>
            awsContext.getLogger.log(
              s"Request succeeded: $operation"
            )
          }
        )

    }

    val response = new APIGatewayV2WebSocketResponse()
    response.setStatusCode(200)
    response.setHeaders(Map("content-type" -> "application/json").asJava)
    response.setBody("""{"status": "ok"}""")
    response
  }
}

