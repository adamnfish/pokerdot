package io.adamnfish.pokerdot

import io.adamnfish.pokerdot.Console.{Direction, Inbound, Outbound, displayId, logConnection, logMessage, noOpConnection, noOpMessage}
import io.adamnfish.pokerdot.models.{AppContext, PlayerAddress}
import io.adamnfish.pokerdot.persistence.DynamoDbDatabase
import io.adamnfish.pokerdot.services.{Clock, DevMessaging, DevRng, DevServerDB}
import io.javalin.Javalin
import org.scanamo.LocalDynamoDB
import zio.{Exit, Unsafe, ZIO}

import java.security.SecureRandom


object DevServer {
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
    println(s"[INFO] initial seed: $initialSeed")
    val rng = new DevRng(initialSeed)

    val messagePrinter: Direction => (String, String) => Unit =
      if (args.contains("--debug")) {
        println("[INFO] debug mode - connection events and messages will be printed")
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
        messagePrinter(Inbound)(wctx.getSessionId, wctx.message)
        val appContext = AppContext(PlayerAddress(wctx.getSessionId), db, messaging, Clock, rng)
        Unsafe.unsafe { implicit unsafe =>
          runtime.unsafe.run {
            PokerDot.pokerdot(wctx.message, appContext)
          }
        } match {
          case Exit.Success(operation) =>
            println(s"[INFO] $operation")
          case Exit.Failure(cause) =>
            cause.failures.foreach { fs =>
              println(s"[ERROR] error: ${fs.logString}")
              fs.exception.foreach { e =>
                println(s"[ERROR] exception: ${e.printStackTrace()}")
              }
            }
            cause.defects.foreach { err =>
              println(s"[ERROR] Fatal error: ${err.toString}")
            }
        }
      }
    })

    Runtime.getRuntime.addShutdownHook(new Thread(() => {
      println("[INFO] Stopping...")
      app.stop()
    }))
  }
}
