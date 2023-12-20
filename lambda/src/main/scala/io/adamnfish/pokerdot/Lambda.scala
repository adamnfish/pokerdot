package io.adamnfish.pokerdot


import com.amazonaws.services.lambda.runtime.events.{APIGatewayV2WebSocketEvent, APIGatewayV2WebSocketResponse}
import com.amazonaws.services.lambda.runtime.{Context => AwsContext}
import com.amazonaws.xray.AWSXRay
import com.amazonaws.xray.entities.Subsegment
import com.typesafe.scalalogging.LazyLogging
import io.adamnfish.pokerdot.models.{AppContext, PlayerAddress, TraceId}
import io.adamnfish.pokerdot.persistence.DynamoDbDatabase
import io.adamnfish.pokerdot.services.{Clock, RandomRng}
import software.amazon.awssdk.auth.credentials.EnvironmentVariableCredentialsProvider
import software.amazon.awssdk.http.urlconnection.UrlConnectionHttpClient
import software.amazon.awssdk.regions.Region
import software.amazon.awssdk.services.apigatewaymanagementapi.ApiGatewayManagementApiClient
import software.amazon.awssdk.services.dynamodb.DynamoDbClient
import zio.{Exit, Runtime, Unsafe, ZIO}

import java.net.URI
import scala.jdk.CollectionConverters._
import scala.util.Properties


class Lambda extends LazyLogging {
  // initialise everything on every request so that ENV vars can be updated without restarting the lambda
  val appContextBuilder: (PlayerAddress, TraceId) => AppContext = {
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
    } yield { (playerAddress: PlayerAddress, traceId: TraceId) =>
      val messaging = new AwsMessaging(apiGatewayManagementClient, traceId)
      AppContext(playerAddress, traceId, db, messaging, Clock, rng)
    }).fold(
      { errMsg =>
        throw new RuntimeException(errMsg)
      },
      identity
    )
  }

  def handleRequest(event: APIGatewayV2WebSocketEvent, awsContext: AwsContext): APIGatewayV2WebSocketResponse = {
    // This doesn't appear in X-Ray, but it's useful to get the trace ID for the log lines
    val subsegment = AWSXRay.beginSubsegment("io.adamnfish.pokerdot.Lambda::handleRequest")
    val handlerTraceId = AWSXRay.currentFormattedId()
    logger.info(s"<$handlerTraceId> route: ${event.getRequestContext.getRouteKey}")
    // Debugging
    // logger.debug(s"<$handlerTraceId> request body: ${event.getBody}")
    // logger.debug(s"<$handlerTraceId> connection ID: ${event.getRequestContext.getConnectionId}")

    event.getRequestContext.getRouteKey match {
      case "$connect" =>
        // ignore this for now
      case "$disconnect" =>
        // ignore this for now
      case "$default" =>
        val traceId = AWSXRay.currentFormattedId()

        val playerAddress = PlayerAddress(event.getRequestContext.getConnectionId)
        val appContext = appContextBuilder(playerAddress, TraceId(traceId))

        Unsafe.unsafe { implicit unsafe =>
          Runtime.default.unsafe.run(
            PokerDot.pokerdot(event.getBody, appContext)
          )
        } match {
          case Exit.Success(operation) =>
            logger.info(s"<$traceId> completed $operation")
            subsegment.putAnnotation("operation", operation)
          case Exit.Failure(cause) =>
            cause.failures.foreach { fs =>
              logger.error(s"<$traceId> error: ${fs.logString}")
              fs.exception match {
                case Some(e) =>
                  logger.error(s"<$traceId> exception: ${e.getMessage}", e)
                  subsegment.addException(e)
                case None =>
                  subsegment.setFault(true)
              }
            }
            cause.defects.foreach { err =>
              logger.error(s"<$traceId> Fatal error: ${err.getMessage}", err)
              subsegment.addException(err)
            }
        }
        logger.info(s"<$traceId> Finished handling request")
    }

    // This doesn't appear in X-Ray
    subsegment.end()
    AWSXRay.endSubsegment(subsegment)
    AWSXRay.sendSubsegment(subsegment)

    val response = new APIGatewayV2WebSocketResponse()
    response.setStatusCode(200)
    response.setHeaders(Map("content-type" -> "application/json").asJava)
    response.setBody("")
    response
  }
}
