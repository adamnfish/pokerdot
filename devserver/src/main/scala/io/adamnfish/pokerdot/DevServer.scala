package io.adamnfish.pokerdot

import cats.effect.unsafe.implicits.global
import cats.effect.*
import io.adamnfish.pokerdot.Console.*
import io.adamnfish.pokerdot.models.{
  AppContext,
  Failures,
  PlayerAddress,
  TraceId
}
import io.adamnfish.pokerdot.persistence.DynamoDbDatabase
import io.adamnfish.pokerdot.services.*
import io.javalin.Javalin
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import software.amazon.awssdk.auth.credentials.{
  AwsBasicCredentials,
  StaticCredentialsProvider
}
import software.amazon.awssdk.regions.Region
import software.amazon.awssdk.services.dynamodb.DynamoDbAsyncClient

import java.net.URI
import java.security.SecureRandom
import java.util.UUID

object CatsDevServer extends IOApp:
  implicit def logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  private def components(
      args: List[String]
  ): Resource[IO, DevServerComponents] =
    for
      messagePrinter <-
        (if (args.contains("--debug")) {
           for _ <- logger.info(
               "debug mode - connection events and messages will be printed"
             )
           yield logMessage[IO]
         } else {
           IO.pure(noOpMessage[IO])
         }).toResource
      connectionPrinter =
        if (args.contains("--debug")) {
          logConnection[IO]
        } else {
          noOpConnection[IO]
        }
      messaging <- IO(new DevMessaging(messagePrinter(Outbound))).toResource

      initialSeed <- args
        .filterNot(_ == "--debug")
        .headOption
        .fold(IO.pure(0L)) { seed =>
          if (seed.toLowerCase == "rng")
            IO(new SecureRandom().nextLong())
          else
            IO.pure(seed.toLong)
        }
        .toResource
      rng = new DevRng[IO](initialSeed)

      client <- Resource.make(IO.blocking {
        DynamoDbAsyncClient
          .builder()
          .endpointOverride(URI.create("http://localhost:8042"))
          .region(Region.US_EAST_1) // not used for local dynamodb, but required
          .credentialsProvider(
            StaticCredentialsProvider.create(
              AwsBasicCredentials.create("dummykey", "dummysecret")
            )
          )
          .build()
      })(client => IO.blocking(client.close()))
      _ <- DevServerDB.createGamesTable(client).toResource
      _ <- DevServerDB.createPlayersTable(client).toResource
      db = new DynamoDbDatabase[IO](client, "games", "players")
      time = new RealTime[IO]

      appContextBuilder =
        (address: PlayerAddress, traceId: TraceId) =>
          AppContext(address, traceId, db, messaging, time, rng)

      app <- Resource.make {
        IO.blocking {
          Javalin.create { config =>
            config.routes.ws(
              "/api",
              { ws =>
                ws.onConnect { wctx =>
                  val result = for {
                    _ <- connectionPrinter(wctx.sessionId, true)
                    _ <- IO.blocking(messaging.connect(wctx))
                  } yield ()
                  result.unsafeRunSync()
                }
                ws.onClose { wctx =>
                  val result = for {
                    _ <- connectionPrinter(wctx.sessionId, false)
                    _ <- IO.blocking(messaging.disconnect(wctx))
                  } yield ()
                  result.unsafeRunSync()
                }
                ws.onMessage { wctx =>
                  val result =
                    for
                      _ <- messagePrinter(Inbound)(
                        wctx.sessionId,
                        wctx.message()
                      )
                      traceId <- IO(TraceId(UUID.randomUUID().toString))
                      appContext = appContextBuilder(
                        PlayerAddress(wctx.sessionId),
                        traceId
                      )
                      operation <- PokerDot.pokerdot(wctx.message(), appContext)
                      _ <- logger.info(s"completed $operation")
                    yield ()
                  result
                    .onError {
                      case failures: Failures =>
                        logger.error(failures)(s"error: ${failures.logString}")
                      case err =>
                        logger.error(err)(s"exception: ${err.getMessage}")
                    }
                    .unsafeRunSync()
                }
              }
            )
          }
        }
      } { app =>
        IO.blocking(app.stop())
      }
    yield DevServerComponents(
      app,
      appContextBuilder,
      messagePrinter,
      connectionPrinter,
      messaging
      // TODO add separate connection manager here?
    )

  override def run(args: List[String]): IO[ExitCode] =
    components(args).use: components =>
      IO.blocking(components.app.start(7000)) >>
        IO.never.as(ExitCode.Success)

case class DevServerComponents(
    app: Javalin,
    appContextBuilder: (PlayerAddress, TraceId) => AppContext[IO],
    messagePrinter: Direction => (String, String) => IO[Unit],
    connectionPrinter: (String, Boolean) => IO[Unit],
    messaging: DevMessaging[IO]
)
