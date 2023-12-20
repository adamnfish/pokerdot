package io.adamnfish.pokerdot

import com.typesafe.scalalogging.LazyLogging
import io.adamnfish.pokerdot.Console.{Direction, Inbound, Outbound, displayId, logConnection, logMessage, noOpConnection, noOpMessage}
import io.adamnfish.pokerdot.models.{AppContext, PlayerAddress, TraceId}
import io.adamnfish.pokerdot.persistence.DynamoDbDatabase
import io.adamnfish.pokerdot.services.{Clock, DevMessaging, DevRng, DevServerDB}
import io.javalin.Javalin
import org.scanamo.LocalDynamoDB
import zio.{Exit, Unsafe, ZIO}

import java.security.SecureRandom
import java.util.UUID


object DevServer extends LazyLogging {
  val client = LocalDynamoDB.syncClient()
  val db = new DynamoDbDatabase(client, "games", "players")
  DevServerDB.createGamesTable(client)
  DevServerDB.createPlayersTable(client)

  def main(args: Array[String]): Unit = {
    val runtime = zio.Runtime.default

    // initials seed defaults to 0, but can be changed at server start time
    val initialSeed = args.filterNot(_ == "--debug").headOption
      .map { seed =>
        if (seed.toLowerCase == "rng")
          new SecureRandom().nextLong()
        else
          seed.toLong
      }
      .getOrElse(0L)
    logger.info(s"initial seed: $initialSeed")
    val rng = new DevRng(initialSeed)

    val messagePrinter: Direction => (String, String) => Unit =
      if (args.contains("--debug")) {
        logger.info("debug mode - connection events and messages will be printed")
        logMessage
      } else {
        noOpMessage
      }
    val connectionPrinter: (String, Boolean) => Unit =
      if (args.contains("--debug")) {
        logConnection
      } else {
        noOpConnection
      }

    val messaging = new DevMessaging(messagePrinter(Outbound))

    val app = Javalin.create()
    app.start(7000)
    app.ws("/api", { ws =>
      ws.onConnect { wctx =>
        val id = messaging.connect(wctx)
        connectionPrinter(id, true)
      }
      ws.onClose { wctx =>
        messaging.disconnect(wctx)
        connectionPrinter(wctx.getSessionId, false)
      }
      ws.onMessage { wctx =>
        val traceId = TraceId(UUID.randomUUID().toString)

        messagePrinter(Inbound)(wctx.getSessionId, wctx.message)
        val appContext = AppContext(PlayerAddress(wctx.getSessionId), traceId, db, messaging, Clock, rng)
        Unsafe.unsafe { implicit unsafe =>
          runtime.unsafe.run {
            PokerDot.pokerdot(wctx.message, appContext)
          }
        } match {
          case Exit.Success(operation) =>
            logger.info(s"completed $operation")
          case Exit.Failure(cause) =>
            cause.failures.foreach { fs =>
              logger.error(s"error: ${fs.logString}")
              fs.exception.foreach { e =>
                logger.error(s"exception: ${e.printStackTrace()}")
              }
            }
            cause.defects.foreach { err =>
              logger.error(s"Fatal error: ${err.getMessage}", err)
            }
        }
      }
    })

    Runtime.getRuntime.addShutdownHook(new Thread(() => {
      logger.info("Stopping...")
      app.stop()
    }))
  }
}
