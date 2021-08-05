package io.adamnfish.pokerdot.logic

import io.adamnfish.pokerdot.logic.Cards.RichRank
import io.adamnfish.pokerdot.logic.Games.{newGame, newPlayer, newSpectator}
import io.adamnfish.pokerdot.logic.Representations._
import io.adamnfish.pokerdot.models.{Ace, Clubs, GameId, Hole, PlayerAddress, Queen, Spades}
import io.adamnfish.pokerdot.{TestDates, TestHelpers}
import org.scalacheck.Gen
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
    "returns player db for each provided player" in {
      forAll(Gen.choose(1, 10)) { n =>
        val players = (0 until n).map { i =>
          newPlayer(GameId("game-id"), s"player-$i", false, PlayerAddress(s"pa-$i"), TestDates)
        }.toList
        allPlayerDbs(players).length shouldEqual n
      }
    }

    "returns correct player db for provided player" in {
      val player = newPlayer(GameId("game-id"), s"player", false, PlayerAddress(s"pa"), TestDates)
      val expected = playerToDb(player)
      allPlayerDbs(List(player)) shouldEqual List(expected)
    }
  }

  "activePlayerDbs" - {
    val p1 = newPlayer(GameId("game-id"), "player-1", false, PlayerAddress("pa-1"), TestDates)
    val p2 = newPlayer(GameId("game-id"), "player-2", false, PlayerAddress("pa-2"), TestDates)
    val p3 = newPlayer(GameId("game-id"), "player-3", false, PlayerAddress("pa-3"), TestDates)

    "includes active players" in {
      activePlayerDbs(List(
        p1,
        p2,
        p3,
      )).map(_.screenName) should contain.allOf("player-1", "player-2", "player-3")
    }

    "does not include a folded player" in {
      activePlayerDbs(List(
        p1,
        p2,
        p3.copy(folded = true),
      )).map(_.screenName).toSet should not contain("player-3")
    }

    "does not include a busted player" in {
      activePlayerDbs(List(
        p1,
        p2.copy(busted = true),
        p3,
      )).map(_.screenName) should not contain("player-2")
    }
  }

  "filteredPlayerDbs" - {
    val p1 = newPlayer(GameId("game-id"), "player-1", false, PlayerAddress("pa-1"), TestDates)
    val p2 = newPlayer(GameId("game-id"), "player-2", false, PlayerAddress("pa-2"), TestDates)
    val p3 = newPlayer(GameId("game-id"), "player-3", false, PlayerAddress("pa-3"), TestDates)

    "includes players in the provided set" in {
      val result = filteredPlayerDbs(List(p1, p2, p3), Set(p2.playerId, p3.playerId)).value.map(_.playerId)
      result shouldEqual List(p2.playerId.pid, p3.playerId.pid)
    }

    "works if allow list contains entries that are not in the provided list" in {
      val result = filteredPlayerDbs(List(p1, p2), Set(p2.playerId, p3.playerId)).value.map(_.playerId)
      result shouldEqual List(p2.playerId.pid)
    }
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
