package io.adamnfish.pokerdot


import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.amazonaws.services.lambda.runtime.Context as AwsContext
import com.amazonaws.services.lambda.runtime.events.{APIGatewayV2WebSocketEvent, APIGatewayV2WebSocketResponse}
import com.amazonaws.xray.AWSXRay
import io.adamnfish.pokerdot.models.{AppContext, PlayerAddress, TraceId}
import io.adamnfish.pokerdot.persistence.DynamoDbDatabase
import io.adamnfish.pokerdot.services.{RandomRng, RealTime}
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import software.amazon.awssdk.auth.credentials.EnvironmentVariableCredentialsProvider
import software.amazon.awssdk.http.crt.AwsCrtAsyncHttpClient
import software.amazon.awssdk.http.urlconnection.UrlConnectionHttpClient
import software.amazon.awssdk.regions.Region
import software.amazon.awssdk.services.apigatewaymanagementapi.ApiGatewayManagementApiClient
import software.amazon.awssdk.services.dynamodb.DynamoDbAsyncClient

import java.time.Duration
import java.net.URI
import scala.jdk.CollectionConverters.*
import scala.util.Properties


class Lambda:
  implicit def logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  // Allocate AWS clients once at class init (Lambda cold start) and reuse across warm invocations.
  // Lambda manages the JVM lifecycle so we don't need to worry about cleanup.
  private val appContextBuilder: (PlayerAddress, TraceId) => AppContext[IO] = {
    val gamesTableName = Properties.envOrElse("GAMES_TABLE", throw new RuntimeException("GAMES_TABLE not set"))
    val playersTableName = Properties.envOrElse("PLAYERS_TABLE", throw new RuntimeException("PLAYERS_TABLE not set"))
    val region = Region.of(Properties.envOrElse("REGION", throw new RuntimeException("REGION not set")))
    val apiGatewayEndpoint = URI.create(
      s"https://${Properties.envOrElse("API_ORIGIN_LOCATION", throw new RuntimeException("API_ORIGIN_LOCATION not set"))}"
    )

    // TODO: maybe use this Async http client for all SDKs and switch to async everywhere?
    val crtAsyncHttpClient = AwsCrtAsyncHttpClient.builder()
      .connectionTimeout(Duration.ofSeconds(3))
      .maxConcurrency(100)
      .build()

    val dynamoDbClient = DynamoDbAsyncClient.builder()
      .region(region)
      .httpClient(crtAsyncHttpClient)
      .credentialsProvider(EnvironmentVariableCredentialsProvider.create())
      .build()

    val apiGatewayManagementApiClient = ApiGatewayManagementApiClient.builder()
      .region(region)
      .httpClient(UrlConnectionHttpClient.create())
      .credentialsProvider(EnvironmentVariableCredentialsProvider.create())
      .endpointOverride(apiGatewayEndpoint)
      .build()

    val database = new DynamoDbDatabase[IO](dynamoDbClient, gamesTableName, playersTableName)
    val time = new RealTime[IO]
    val rng = new RandomRng[IO]

    (playerAddress, traceId) =>
      val messaging = new AwsMessaging[IO](apiGatewayManagementApiClient, traceId)
      AppContext(playerAddress, traceId, database, messaging, time, rng)
  }

  def handleRequest(event: APIGatewayV2WebSocketEvent, context: AwsContext): APIGatewayV2WebSocketResponse =
    program(event, context).unsafeRunSync()

  def program(event: APIGatewayV2WebSocketEvent, context: AwsContext): IO[APIGatewayV2WebSocketResponse] =
      for
        subsegment <- IO.blocking(AWSXRay.beginSubsegment("io.adamnfish.pokerdot.Lambda::handleRequest"))
        traceId <- IO.blocking(AWSXRay.currentFormattedId())
        _ <- logger.info(s"<$traceId> route: ${event.getRequestContext.getRouteKey}")
        
        _ <- event.getRequestContext.getRouteKey match
          case "$connect" =>
            // ignore this for now
            IO.unit
          case "$disconnect" =>
            // ignore this for now
            IO.unit
          case "$default" =>
            val playerAddress = PlayerAddress(event.getRequestContext.getConnectionId)
            val appContext = appContextBuilder(playerAddress, TraceId(traceId))
            for 
              operation <- PokerDot.pokerdot[IO](event.getBody, appContext)
                .onError { e =>
                  logger.error(e)(s"<$traceId> Error: ${e.getMessage}") &>
                    IO.blocking(subsegment.addException(e))
                }
              _ <- logger.info(s"<$traceId> completed $operation")
              _ <- IO.blocking(subsegment.putAnnotation("operation", operation))
            yield
              ()
          case routeKey =>
            logger.error(s"<$traceId> Unhandled route: $routeKey")
  
        _ <- logger.info(s"<$traceId> Finished handling request")
  
        _ <- IO.blocking {
          subsegment.end()
          AWSXRay.endSubsegment(subsegment)
          AWSXRay.sendSubsegment(subsegment)
        }
      yield
        val response = new APIGatewayV2WebSocketResponse()
        response.setStatusCode(200)
        response.setHeaders(Map("content-type" -> "application/json").asJava)
        response.setBody("")
        response
