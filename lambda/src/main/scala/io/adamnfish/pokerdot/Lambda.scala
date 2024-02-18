package io.adamnfish.pokerdot


import cats.effect.std.Env
import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Resource}
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
import software.amazon.awssdk.services.dynamodb.{DynamoDbAsyncClient, DynamoDbClient}

import java.time.Duration
import java.net.URI
import scala.jdk.CollectionConverters.*
import scala.util.Properties


class Lambda:
  implicit def logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  val app: Resource[IO, (PlayerAddress, TraceId) => AppContext[IO]] =
    for
      // DB table names
      gamesTableName <- Env[IO].get("GAMES_TABLE").flatMap {
        case Some(gamesTableName) => IO.pure(gamesTableName)
        case None => IO.raiseError(new RuntimeException("GAMES_TABLE not set"))
      }.toResource
      playersTableName <- Env[IO].get("PLAYERS_TABLE").flatMap {
        case Some(playersTableName) => IO.pure(playersTableName)
        case None => IO.raiseError(new RuntimeException("PLAYERS_TABLE not set"))
      }.toResource
      // TODO: maybe use this Async http client for all SDKs and switch to async everywhere?
      crtAsyncHttpClient <- Resource.make(IO {
        AwsCrtAsyncHttpClient.builder()
          .connectionTimeout(Duration.ofSeconds(3))
          .maxConcurrency(100)
          .build()
      })(client => IO(client.close()))
      // AWS clients
      dynamoDbClient <- Resource.make(IO {
        DynamoDbAsyncClient.builder()
          .region(Region.EU_WEST_1)
          .httpClient(crtAsyncHttpClient)
          .credentialsProvider(EnvironmentVariableCredentialsProvider.create())
          .build()
      })(client => IO(client.close()))
      apiGatewayManagementApiClient <- Resource.make(IO {
        ApiGatewayManagementApiClient.builder()
          .region(Region.EU_WEST_1)
          .httpClient(UrlConnectionHttpClient.create())
          .credentialsProvider(EnvironmentVariableCredentialsProvider.create())
          .endpointOverride(URI.create(Properties.envOrElse("APIGATEWAY_ENDPOINT", "http://localhost:3001")))
          .build()
      })(client => IO(client.close()))
      // create services
      database = new DynamoDbDatabase[IO](dynamoDbClient, gamesTableName, playersTableName)
      time = new RealTime[IO]
      rng = new RandomRng[IO]
    yield (playerAddress, traceId) =>
      val messaging = new AwsMessaging[IO](apiGatewayManagementApiClient, traceId)
      AppContext(playerAddress, traceId, database, messaging, time, rng)
  
  def handleRequest(event: APIGatewayV2WebSocketEvent, context: AwsContext): APIGatewayV2WebSocketResponse =
    program(event, context).unsafeRunSync()

  def program(event: APIGatewayV2WebSocketEvent, context: AwsContext): IO[APIGatewayV2WebSocketResponse] =
    app.use: appContextBuilder =>
      for
        subsegment <- IO(AWSXRay.beginSubsegment("io.adamnfish.pokerdot.Lambda::handleRequest"))
        traceId <- IO(AWSXRay.currentFormattedId())
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
                .onError: e =>
                  logger.error(e)(s"<$traceId> Error: ${e.getMessage}") &>
                    IO.blocking(subsegment.addException(e))
              _ <- logger.info(s"<$traceId> completed $operation")
              _ <- IO(subsegment.putAnnotation("operation", operation))
            yield
              ()
          case routeKey =>
            logger.error(s"<$traceId> Unhandled route: $routeKey")
  
        _ <- logger.info(s"<$traceId> Finished handling request")
  
        _ <- IO:
          subsegment.end()
          AWSXRay.endSubsegment(subsegment)
          AWSXRay.sendSubsegment(subsegment)
      yield
        val response = new APIGatewayV2WebSocketResponse()
        response.setStatusCode(200)
        response.setHeaders(Map("content-type" -> "application/json").asJava)
        response.setBody("")
        response
