package io.adamnfish.pokerdot


import com.amazonaws.services.lambda.runtime.events.{APIGatewayV2WebSocketEvent, APIGatewayV2WebSocketResponse}
import com.amazonaws.services.lambda.runtime.{Context => AwsContext}
import io.adamnfish.pokerdot.models.{AppContext, PlayerAddress}
import io.adamnfish.pokerdot.persistence.DynamoDbDatabase
import io.adamnfish.pokerdot.services.{Dates, RandomRng}
import software.amazon.awssdk.auth.credentials.EnvironmentVariableCredentialsProvider
import software.amazon.awssdk.http.urlconnection.UrlConnectionHttpClient
import software.amazon.awssdk.regions.Region
import software.amazon.awssdk.services.apigatewaymanagementapi.ApiGatewayManagementApiClient
import software.amazon.awssdk.services.dynamodb.DynamoDbClient

import java.net.URI
import scala.jdk.CollectionConverters._
import scala.util.Properties


class Lambda {
  // initialise AWS clients at start time
  val appContextBuilder: (PlayerAddress, AwsContext) => AppContext = {
    (for {
      // AWS ASK configuration
      regionStr <- Properties.envOrNone("REGION")
        .toRight("region not configured")
      region = Region.of(regionStr)
      // API Gateway client configuration
      apiGatewayEndpointStr <- Properties.envOrNone("API_ORIGIN_LOCATION")
        .toRight("API Gateway endpoint name not configured")
      apiGatewayEndpointUri = new URI(s"https://$apiGatewayEndpointStr")
      // table names
      gamesTableName <- Properties.envOrNone("GAMES_TABLE")
        .toRight("games table name not configured")
      playersTableName <- Properties.envOrNone("PLAYERS_TABLE")
        .toRight("players table name not configured")
      // create SDK clients
      apiGatewayManagementClient = ApiGatewayManagementApiClient.builder()
        .endpointOverride(apiGatewayEndpointUri)
        .region(region)
        .credentialsProvider(EnvironmentVariableCredentialsProvider.create())
        .httpClientBuilder(UrlConnectionHttpClient.builder())
        .build()
      dynamoDbClient = DynamoDbClient.builder()
        .region(region)
        .credentialsProvider(EnvironmentVariableCredentialsProvider.create())
        .httpClientBuilder(UrlConnectionHttpClient.builder())
        .build()
      db = new DynamoDbDatabase(dynamoDbClient, gamesTableName, playersTableName)
      rng = new RandomRng
    } yield { (playerAddress: PlayerAddress, awsContext: AwsContext) =>
      val messaging = new AwsMessaging(apiGatewayManagementClient, awsContext.getLogger)
      AppContext(playerAddress, db, messaging, Dates, rng)
    }).fold(
      { errMsg =>
        throw new RuntimeException(errMsg)
      },
      identity
    )
  }

  def handleRequest(event: APIGatewayV2WebSocketEvent, awsContext: AwsContext): APIGatewayV2WebSocketResponse = {
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
        val appContext = appContextBuilder(playerAddress, awsContext)

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
            cause.defects.foreach { err =>
              awsContext.getLogger.log(s"Fatal error: ${err.toString}")
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

