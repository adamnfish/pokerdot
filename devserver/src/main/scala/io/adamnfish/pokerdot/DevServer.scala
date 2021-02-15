package io.adamnfish.pokerdot

import io.adamnfish.pokerdot.Console.displayId
import io.adamnfish.pokerdot.models.{AppContext, PlayerAddress}
import io.adamnfish.pokerdot.persistence.DynamoDbDatabase
import io.adamnfish.pokerdot.services.{Dates, DevMessaging, DevRng, DevServerDB}
import io.javalin.Javalin
import org.scanamo.LocalDynamoDB
import zio.IO


object DevServer {
  val messaging = new DevMessaging(logMessage)
  val client = LocalDynamoDB.syncClient()
  val db = new DynamoDbDatabase(client, "games", "players")
  DevServerDB.createGamesTable(client)
  DevServerDB.createPlayersTable(client)

  def main(args: Array[String]): Unit = {
    val runtime = zio.Runtime.default

    val app = Javalin.create()
    app.start(7000)

    // initials seed defaults to 0, but can be changed at server start time
    val initialSeed = args.headOption.map(_.toLong).getOrElse(0L)
    val rng = new DevRng(initialSeed)

    app.ws("/api", { ws =>
      ws.onConnect { wctx =>
        val id = messaging.connect(wctx)
        println(s"Connected: ${displayId(id, fullId = true)}")
      }
      ws.onClose { wctx =>
        messaging.disconnect(wctx)
        println(s"Disconnected: ${displayId(wctx.getSessionId)}")
      }
      ws.onMessage { wctx =>
        println(s"Message: ${displayId(wctx.getSessionId)} <- ${wctx.message}")
        val appContext = AppContext(PlayerAddress(wctx.getSessionId), db, messaging, Dates, rng)
        val program = PokerDot.pokerdot(wctx.message, appContext).catchAll { failures =>
          IO {
            println(s"[ERROR] Failures: ${failures.logString}")
            "FAILURE"
          }
        }

        runtime.unsafeRunSync(program).fold(
          { cause =>
            println(s"[ERROR] ${cause.prettyPrint}")
            cause.failures.foreach { e =>
              println(s"[ERROR] Unhandled exception: ${e.printStackTrace()}")
            }
            cause.defects.foreach { err =>
              println(s"[ERROR] Fatal error: ${err.toString}")
            }
          },
          { operation =>
            println(s"[INFO] Operation $operation completed")
          }
        )
      }
    })

    Runtime.getRuntime.addShutdownHook(new Thread(() => {
      println("[INFO] Stopping...")
      app.stop()
    }))
  }

  def logMessage(uid: String, body: String): Unit = {
    println(s"Message: ${displayId(uid)} -> $body")
  }
}
