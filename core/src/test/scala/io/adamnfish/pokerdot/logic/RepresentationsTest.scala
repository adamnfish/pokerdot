package io.adamnfish.pokerdot.logic

import io.adamnfish.pokerdot.logic.Cards.RichRank
import io.adamnfish.pokerdot.logic.Games.{newGame, newPlayer, newSpectator}
import io.adamnfish.pokerdot.logic.Representations._
import io.adamnfish.pokerdot.models.{Ace, Clubs, GameId, Hole, PlayerAddress, Queen, Spades}
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
    "hole" - {
      val hole = Hole(Queen of Clubs, Ace of Spades)
      val player = newPlayer(GameId("game-id"), "screen name", false, PlayerAddress("player-address"), TestDates)

      "is included if present, even if the hole is not visible" in {
        summariseSelf(
          player.copy(
            hole = Some(hole),
            holeVisible = false,
          )
        ).hole shouldEqual Some(hole)
      }

      "is included if present, when hole is marked is visible" in {
        summariseSelf(
          player.copy(
            hole = Some(hole),
            holeVisible = true,
          )
        ).hole shouldEqual Some(hole)
      }

      "is empty if the self player has no hole" in {
        summariseSelf(
          player.copy(
            hole = None,
            holeVisible = true,
          )
        ).hole shouldEqual None
      }
    }

    "TODO: more tests" ignore {}
  }

  "summarisePlayer" - {
    "hole" - {
      val hole = Hole(Queen of Clubs, Ace of Spades)
      val player = newPlayer(GameId("game-id"), "screen name", false, PlayerAddress("player-address"), TestDates)
        .copy(
          hole = Some(hole),
        )

      "is included if the hole is marked as visible" in {
        summarisePlayer(
          player.copy(holeVisible = true)
        ).hole shouldEqual Some(hole)
      }

      "is omitted if marked as not visible" in {
        summarisePlayer(
          player.copy(holeVisible = false)
        ).hole shouldEqual None
      }
    }

    "TODO: more tests" ignore {}
  }

  "summariseSpectator" - {
    "TODO" ignore {}
  }
}
