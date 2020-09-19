package io.adamnfish.pokerdot.logic

import io.adamnfish.pokerdot.TestHelpers
import io.adamnfish.pokerdot.logic.Play.generateRound
import io.adamnfish.pokerdot.models.{GameId, JoinGame, PlayerAddress, PlayerKey, PreFlop}
import io.adamnfish.pokerdot.logic.Games._
import org.scalatest.EitherValues
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks


class GamesTest extends AnyFreeSpec with Matchers with ScalaCheckDrivenPropertyChecks with TestHelpers with EitherValues {
  "newGame" - {
    "initialises the basic fields correctly" in {
      forAll { (gameName: String, trackStacks: Boolean, seed: Long) =>
        newGame(gameName, trackStacks).value(seed) should have(
          "gameName" as gameName,
          "players" as Nil,
          "spectators" as Nil,
          "inTurn" as None,
          "button" as 0,
          "started" as false,
          "trackStacks" as trackStacks,
          "timer" as None,
        )
      }
    }

    "sets roundType to pre-flop" in {
      val game = newGame("gameName", true).value(12345L)
      game.round.phase shouldEqual PreFlop
    }

    "cards used for this round are equal if the seed is equal" in {
      forAll { (seed: Long) =>
        val game1 = newGame("gameName", false).value(seed)
        val game2 = newGame("gameName", false).value(seed)
        game1.round should have(
          "burn1" as game2.round.burn1,
          "flop1" as game2.round.flop1,
          "flop2" as game2.round.flop2,
          "flop3" as game2.round.flop3,
          "burn2" as game2.round.burn2,
          "turn" as game2.round.turn,
          "burn3" as game2.round.burn3,
          "river" as game2.round.river,
        )
      }
    }

    "cards used for this round are (probably) different if the seed is different" in {
      // less likely to have clashing cards from multiple distinct seeds
      // this helps prevent intermittent failures when the generator happens to stumble across a clash
      forAll { (seed1: Long, seed2: Long, seed3: Long) =>
        whenever(Set(seed1, seed2, seed3).size == 3) {
          val game1 = newGame("gameName", false).value(seed1)
          val game2 = newGame("gameName", false).value(seed2)
          val game3 = newGame("gameName", false).value(seed3)
          val game4 = newGame("gameName", false).value(seed1 + 1)
          val distinctRounds = Set(game1.round, game2.round, game3.round, game4.round)
          distinctRounds.size > 1
        }
      }
    }

    "cards can be regenerated from the current game seed" in {
      forAll { (seed: Long) =>
        val game = newGame("gameName", false).value(seed)
        val regeneratedRound = generateRound(PreFlop).value(game.seed)
        game.round shouldEqual regeneratedRound
      }
    }
  }

  "newPlayer" - {
    "initialises basic fields correctly" in {
      forAll { (gid: String, screenName: String, isCreator: Boolean, address: String) =>
        val player = newPlayer(GameId(gid), screenName, isCreator, PlayerAddress(address))
        player should have(
          "gameId" as gid,
          "playerAddress" as address,
          "screenName" as screenName,
          "stack" as 0,
          "pot" as 0,
          "bid" as 0,
          "folded" as false,
          "busted" as false,
          "hole" as None,
          "isCreator" as isCreator,
        )
      }
    }

    "produces a different player ID each time it is called" in {
      val player1 = newPlayer(GameId("gid"), "screenName1", false, PlayerAddress("address1"))
      val player2 = newPlayer(GameId("gid"), "screenName2", false, PlayerAddress("address2"))
      player1.playerId should not equal player2.playerId
    }

    "produces a different player key each time it is called" in {
      val player1 = newPlayer(GameId("gid"), "screenName1", false, PlayerAddress("address1"))
      val player2 = newPlayer(GameId("gid"), "screenName2", false, PlayerAddress("address2"))
      player1.playerKey should not equal player2.playerKey
    }
  }

  "addPlayerIds" - {
    val game = newGame("game name", false).value(123L)
    val gameDb = Representations.gameToDb(game)
    val player1 = newPlayer(game.gameId, "player name", false, PlayerAddress("address 1"))
    val player1Db = Representations.playerToDb(player1)
    val player2 = newPlayer(game.gameId, "player 2 name", false, PlayerAddress("address 2"))
    val player2Db = Representations.playerToDb(player2)

    "includes a passed player's ID" in {
      addPlayerIds(gameDb, List(player1Db)).playerIds should contain(player1Db.playerId)
    }

    "includes multiple passed player's IDs" in {
      val playerIds = addPlayerIds(gameDb, List(player1Db, player2Db)).playerIds
      playerIds should contain.allOf(player1Db.playerId, player2Db.playerId)
    }

    "includes previous player IDs as well as new ones" in {
      val allPlayerIds =
        addPlayerIds(
          addPlayerIds(gameDb, List(player1Db)),
          List(player2Db)
        ).playerIds
      allPlayerIds should contain.allOf(player1Db.playerId, player2Db.playerId)
    }
  }

  "addPlayer" - {
    val game = newGame("game name", false).value(123L)
    val player = newPlayer(game.gameId, "player name", false, PlayerAddress("address"))

    "includes passed player in the game" in {
      addPlayer(game, player).players should contain(player)
    }

    "includes passed player as well as any existing players" in {
      val player2 = newPlayer(game.gameId, "player 2", false, PlayerAddress("address 2"))
      val addedPlayers = addPlayer(addPlayer(game, player), player2).players
      addedPlayers should contain.allOf(player, player2)
    }
  }

  "gameCode" - {
    "is always a prefix of the game's ID" in {
      forAll { (id: String) =>
        val code = gameCode(GameId(id))
        id.startsWith(code) shouldEqual true
      }
    }
  }

  "normalisedGameCode" - {
    "leaves a normal game code alone" in {
      normaliseGameCode(
        JoinGame("abcde", "screen name")
      ).gameCode shouldEqual "abcde"
    }

    "normalises an o (oh)" in {
      normaliseGameCode(
        JoinGame("abcdo", "screen name")
      ).gameCode shouldEqual "abcd0"
    }

    "normalises a capital O (oh)" in {
      normaliseGameCode(
        JoinGame("abcdO", "screen name")
      ).gameCode shouldEqual "abcd0"
    }
  }

  "requireGame" - {
    "returns the gameDb if present" in {
      val gameDb = Representations.gameToDb(
        newGame("game name", false).value(123L)
      )
      requireGame(Some(gameDb), gameDb.gameId).value shouldEqual gameDb
    }

    "fails with a note about the GID, if the game wasn't found" in {
      val failures = requireGame(None, "GID").left.value
      failures.logString should include("GID")
    }
  }

  "ensureNotStarted" - {
    "is successful if the game has not yet started" in {
      val game = newGame("game name", false).value(123L)
      ensureNotStarted(
        game.copy(started = false)
      ).isRight shouldEqual true
    }

    "fails if the game has already started" in {
      val game = newGame("game name", false).value(123L)
      ensureNotStarted(
        game.copy(started = true)
      ).isLeft shouldEqual true
    }
  }

  "ensureNoDuplicateScreenName" - {
    val game = newGame("game name", false).value(123L)

    "is fine for a game with no players" in {
      forAll { screenName: String =>
        ensureNoDuplicateScreenName(game, screenName).isRight shouldEqual true
      }
    }

    "is successful for a screen name that isn't already taken" in {
      forAll { screenName: String =>
        whenever(screenName != "screenname") {
          val player = newPlayer(game.gameId, "screenname", false, PlayerAddress("address"))
          val gameWithPlayer = addPlayer(game, player)
          ensureNoDuplicateScreenName(gameWithPlayer, screenName).isRight shouldEqual true
        }
      }
    }

    "fails for a screen name that is already in use in this game" in {
      forAll { screenName: String =>
        val player = newPlayer(game.gameId, screenName, false, PlayerAddress("address"))
        val gameWithPlayer = addPlayer(game, player)
        ensureNoDuplicateScreenName(gameWithPlayer, screenName).isLeft shouldEqual true
      }
    }
  }

  "ensureNotAlreadyPlaying" - {
    val game = newGame("game name", false).value(123L)

    "succeeds for a player address that isn't already in use" in {
      forAll { address: String =>
        whenever(address != "address1") {
          val player = newPlayer(game.gameId, "screenname", false, PlayerAddress("address1"))
          val gameWithPlayer = addPlayer(game, player)
          ensureNotAlreadyPlaying(gameWithPlayer, PlayerAddress(address)).isRight shouldEqual true
        }
      }
    }

    "fails for a player address that is already being used" in {
      forAll { address: String =>
        val player = newPlayer(game.gameId, "screenname", false, PlayerAddress(address))
        val gameWithPlayer = addPlayer(game, player)
        ensureNotAlreadyPlaying(gameWithPlayer, PlayerAddress(address)).isLeft shouldEqual true
      }
    }
  }

  "ensurePlayerKey" - {
    val game = newGame("game name", false).value(123L)
    val player = newPlayer(game.gameId, "player name", false, PlayerAddress("address"))

    "if the player is part of this game" - {
      val gameWithPlayer = addPlayer(game, player)

      "returns the valid player if their key is valid" in {
        val playerResult = ensurePlayerKey(gameWithPlayer, player.playerId, player.playerKey).value
        playerResult shouldEqual player
      }

      "fails if the player key does not match" in {
        val incorrectPlayerKey = PlayerKey("bad player key")
        val result = ensurePlayerKey(gameWithPlayer, player.playerId, incorrectPlayerKey)
        result.isLeft shouldEqual true
      }
    }

    "fails if the player does not exist in the game" in {
      val result = ensurePlayerKey(game, player.playerId, player.playerKey)
      result.isLeft shouldEqual true
    }
  }
}
