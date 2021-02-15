package io.adamnfish.pokerdot

import io.adamnfish.pokerdot.Console.Direction

import scala.io.AnsiColor
import scala.util.Random

object Console {
  def displayId(id: String, fullId: Boolean = false): String = {
    val seed = id.filterNot(_ == '-').sum
    val rng = new Random(seed)
    val colourCode = rng.shuffle(colours).head
    val idSummary =
      if (fullId) id
      else id.take(8)
    s"$colourCode$idSummary${AnsiColor.RESET}"
  }

  private val colours = List(
    s"${AnsiColor.RED_B}${AnsiColor.BOLD}${AnsiColor.BLACK}",
    s"${AnsiColor.RED_B}${AnsiColor.BOLD}${AnsiColor.GREEN}",
    s"${AnsiColor.RED_B}${AnsiColor.BOLD}${AnsiColor.YELLOW}",
    s"${AnsiColor.RED_B}${AnsiColor.BOLD}${AnsiColor.MAGENTA}",
    s"${AnsiColor.RED_B}${AnsiColor.BOLD}${AnsiColor.CYAN}",
    s"${AnsiColor.RED_B}${AnsiColor.BOLD}${AnsiColor.WHITE}",
    s"${AnsiColor.GREEN_B}${AnsiColor.BOLD}${AnsiColor.BLACK}",
    s"${AnsiColor.GREEN_B}${AnsiColor.BOLD}${AnsiColor.RED}",
    s"${AnsiColor.GREEN_B}${AnsiColor.BOLD}${AnsiColor.YELLOW}",
    s"${AnsiColor.GREEN_B}${AnsiColor.BOLD}${AnsiColor.BLUE}",
    s"${AnsiColor.GREEN_B}${AnsiColor.BOLD}${AnsiColor.MAGENTA}",
    s"${AnsiColor.GREEN_B}${AnsiColor.BOLD}${AnsiColor.WHITE}",
    s"${AnsiColor.YELLOW_B}${AnsiColor.BOLD}${AnsiColor.BLACK}",
    s"${AnsiColor.YELLOW_B}${AnsiColor.BOLD}${AnsiColor.RED}",
    s"${AnsiColor.YELLOW_B}${AnsiColor.BOLD}${AnsiColor.BLUE}",
    s"${AnsiColor.YELLOW_B}${AnsiColor.BOLD}${AnsiColor.MAGENTA}",
    s"${AnsiColor.YELLOW_B}${AnsiColor.BOLD}${AnsiColor.CYAN}",
    s"${AnsiColor.YELLOW_B}${AnsiColor.BOLD}${AnsiColor.WHITE}",
    s"${AnsiColor.BLUE_B}${AnsiColor.BOLD}${AnsiColor.BLACK}",
    s"${AnsiColor.BLUE_B}${AnsiColor.BOLD}${AnsiColor.RED}",
    s"${AnsiColor.BLUE_B}${AnsiColor.BOLD}${AnsiColor.YELLOW}",
    s"${AnsiColor.BLUE_B}${AnsiColor.BOLD}${AnsiColor.CYAN}",
    s"${AnsiColor.BLUE_B}${AnsiColor.BOLD}${AnsiColor.WHITE}",
    s"${AnsiColor.MAGENTA_B}${AnsiColor.BOLD}${AnsiColor.BLACK}",
    s"${AnsiColor.MAGENTA_B}${AnsiColor.BOLD}${AnsiColor.RED}",
    s"${AnsiColor.MAGENTA_B}${AnsiColor.BOLD}${AnsiColor.GREEN}",
    s"${AnsiColor.MAGENTA_B}${AnsiColor.BOLD}${AnsiColor.YELLOW}",
    s"${AnsiColor.MAGENTA_B}${AnsiColor.BOLD}${AnsiColor.CYAN}",
    s"${AnsiColor.MAGENTA_B}${AnsiColor.BOLD}${AnsiColor.WHITE}",
    s"${AnsiColor.CYAN_B}${AnsiColor.BOLD}${AnsiColor.BLACK}",
    s"${AnsiColor.CYAN_B}${AnsiColor.BOLD}${AnsiColor.RED}",
    s"${AnsiColor.CYAN_B}${AnsiColor.BOLD}${AnsiColor.YELLOW}",
    s"${AnsiColor.CYAN_B}${AnsiColor.BOLD}${AnsiColor.MAGENTA}",
    s"${AnsiColor.CYAN_B}${AnsiColor.BOLD}${AnsiColor.WHITE}",
  )

  def logMessage(direction: Direction)(uid: String, body: String): Unit = {
    val dir = direction match {
      case Inbound => "<-"
      case Outbound => "->"
    }
    println(s"[DEBUG] Message: ${displayId(uid)} $dir $body")
  }

  def noOpMessage(direction: Direction)(uid: String, body: String): Unit = {
    ()
  }

  sealed trait Direction
  case object Inbound extends Direction
  case object Outbound extends Direction

  def logConnection(uid: String, connected: Boolean): Unit = {
    if (connected) {
      println(s"[DEBUG] Connected: ${displayId(uid, fullId = true)}")
    } else {
      println(s"[DEBUG] Disconnected: ${displayId(uid)}")
    }
  }

  def noOpConnection(uid: String, connected: Boolean): Unit = {
    ()
  }
}
