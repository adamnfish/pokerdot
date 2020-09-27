package io.adamnfish.pokerdot

import io.adamnfish.pokerdot.Console.displayId
import io.adamnfish.pokerdot.models.{AppContext, PlayerAddress}
import io.adamnfish.pokerdot.persistence.DynamoDb
import io.javalin.Javalin
import org.scanamo.LocalDynamoDB
import zio.IO

import scala.util.control.NonFatal


object DevServer {
  val messaging = new DevMessaging
  val client = LocalDynamoDB.client()
  val db = new DynamoDb(client, "games", "players")
  DevServerDB.createGamesTable(client)
  DevServerDB.createPlayersTable(client)

  def main(args: Array[String]): Unit = {
    val runtime = zio.Runtime.default

    val app = Javalin.create()
    app.start(7000)

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
        val appContext = AppContext(PlayerAddress(wctx.getSessionId), db, messaging)
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
}
