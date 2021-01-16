package io.adamnfish.pokerdot.logic

import io.adamnfish.pokerdot.logic.Games.{newGame, newPlayer, newSpectator}
import io.adamnfish.pokerdot.logic.Representations._
import io.adamnfish.pokerdot.models.{GameId, PlayerAddress}
import io.adamnfish.pokerdot.{TestDates, TestHelpers}
import org.scalatest.EitherValues
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks


class RepresentationsTest extends AnyFreeSpec with Matchers with ScalaCheckDrivenPropertyChecks with TestHelpers with EitherValues {
  "games" - {
    "round trips a game correctly" in {
      val game = newGame("game name", trackStacks = false, TestDates, 1)
      val gameDb = gameToDb(game)
      val reconstructedGame = gameFromDb(gameDb, Nil).value
      reconstructedGame shouldEqual game
    }
  }

  "players" - {
    "round trips a player correctly" in {
      val gameId = GameId("game-id")
      val player = newPlayer(gameId, "player", false, PlayerAddress("player-address"), TestDates)
      val playerDb = playerToDb(player)
      val reconstructedPlayer = playerFromDb(playerDb)
      reconstructedPlayer shouldEqual player
    }
  }

  "spectators" - {
    "round trips a spectator correctly" in {
      val gameId = GameId("game-id")
      val spectator = newSpectator(gameId, "spectator", false, PlayerAddress("player-address"), TestDates)
      val spectatorDb = spectatorToDb(spectator)
      val reconstructedSpectator = spectatorFromDb(spectatorDb)
      reconstructedSpectator shouldEqual spectator
    }
  }

  "allPlayerDbs" - {
    "TODO" ignore {}
  }

  "filteredPlayerDbs" - {
    "TODO" ignore {}
  }

  "gameFromDb" - {
    "TODO" ignore {}
  }

  "playerFromDb" - {
    "TODO" ignore {}
  }

  "spectatorFromDb" - {
    "TODO" ignore {}
  }

  "gameStatus" - {
    "TODO" ignore {}
  }

  "roundWinnings" - {
    "TODO" ignore {}
  }

  "summariseRound" - {
    "TODO" ignore {}
  }

  "summariseGame" - {
    "TODO" ignore {}
  }

  "summariseSelf" - {
    "TODO" ignore {}
  }

  "summarisePlayer" - {
    "TODO" ignore {}
  }

  "summariseSpectator" - {
    "TODO" ignore {}
  }
}
