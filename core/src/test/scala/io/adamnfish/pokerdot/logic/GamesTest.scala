package io.adamnfish.pokerdot.logic

import io.adamnfish.pokerdot.logic.Cards.RichRank
import io.adamnfish.pokerdot.{PokerGenerators, TestClock, TestHelpers}
import io.adamnfish.pokerdot.logic.Play.generateRound
import io.adamnfish.pokerdot.models._
import io.adamnfish.pokerdot.logic.Games._
import org.scalacheck.Gen
import org.scalatest.{EitherValues, OptionValues}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.util.Random


class GamesTest extends AnyFreeSpec with Matchers with ScalaCheckDrivenPropertyChecks with TestHelpers with EitherValues with OptionValues with PokerGenerators {
  "newGame" - {
    "initialises the basic fields correctly" in {
      forAll { (gameName: String, trackStacks: Boolean, seed: Long) =>
        newGame(gameName, trackStacks, TestClock, seed) should have(
          "gameName" as gameName,
          "players" as Nil,
          "spectators" as Nil,
          "inTurn" as None,
          "button" as 0,
          "started" as false,
          "trackStacks" as trackStacks,
          "timer" as None,
          "seed" as seed,
        )
      }
    }

    "sets expiry based on the time generated by the provided Clock implementation" in {
      val game = newGame("game name", false, TestClock, 0)
      game.expiry shouldEqual expiryTime(TestClock.now())
    }

    "sets startTime to the current time as generated by the provided Clock implementation" in {
      val game = newGame("game name", false, TestClock, 0)
      game.startTime shouldEqual TestClock.now()
    }

    "sets roundType to pre-flop" in {
      val game = newGame("gameName", true, TestClock, 12345L)
      game.round.phase shouldEqual PreFlop
    }

    "cards used for this round are equal if the seed is equal" in {
      forAll { (seed: Long) =>
        val game1 = newGame("gameName", false, TestClock, seed)
        val game2 = newGame("gameName", false, TestClock, seed)
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
          val game1 = newGame("gameName", false, TestClock, seed1)
          val game2 = newGame("gameName", false, TestClock, seed2)
          val game3 = newGame("gameName", false, TestClock, seed3)
          val game4 = newGame("gameName", false, TestClock, seed1 + 1)
          val distinctRounds = Set(game1.round, game2.round, game3.round, game4.round)
          distinctRounds.size > 1
        }
      }
    }

    "cards can be regenerated from the current game seed" in {
      forAll { (seed: Long) =>
        val game = newGame("gameName", false, TestClock, seed)
        val regeneratedRound = generateRound(PreFlop, 0, game.seed)
        game.round shouldEqual regeneratedRound
      }
    }
  }

  "newPlayer" - {
    "initialises basic fields correctly" in {
      forAll { (gid: String, screenName: String, isHost: Boolean, address: String) =>
        val player = newPlayer(GameId(gid), screenName, isHost, PlayerAddress(address), TestClock)
        player should have(
          "gameId" as gid,
          "playerAddress" as address,
          "screenName" as screenName,
          "stack" as 0,
          "pot" as 0,
          "bet" as 0,
          "folded" as false,
          "busted" as false,
          "hole" as None,
          "isHost" as isHost,
        )
      }
    }

    "sets expiry based on the time generated by the provided Clock implementation" in {
      val player = newPlayer(GameId("gid"), "screenName1", false, PlayerAddress("address1"), TestClock)
      player.expiry shouldEqual expiryTime(TestClock.now())
    }

    "produces a different player ID each time it is called" in {
      val player1 = newPlayer(GameId("gid"), "screenName1", false, PlayerAddress("address1"), TestClock)
      val player2 = newPlayer(GameId("gid"), "screenName2", false, PlayerAddress("address2"), TestClock)
      player1.playerId should not equal player2.playerId
    }

    "produces a different player key each time it is called" in {
      val player1 = newPlayer(GameId("gid"), "screenName1", false, PlayerAddress("address1"), TestClock)
      val player2 = newPlayer(GameId("gid"), "screenName2", false, PlayerAddress("address2"), TestClock)
      player1.playerKey should not equal player2.playerKey
    }
  }

  "newSpectator" - {
    "initialises basic fields correctly" in {
      forAll { (gid: String, screenName: String, isHost: Boolean, address: String) =>
        val spectator = newSpectator(GameId(gid), screenName, isHost, PlayerAddress(address), TestClock)
        spectator should have(
          "gameId" as gid,
          "playerAddress" as address,
          "screenName" as screenName,
          "isHost" as isHost,
        )
      }
    }

    "sets expiry to the time generated by the provided Clock implementation" in {
      val spectator = newSpectator(GameId("gid"), "screenName1", false, PlayerAddress("address1"), TestClock)
      spectator.expiry shouldEqual expiryTime(TestClock.now())
    }

    "produces a different player ID each time it is called" in {
      val spectator1 = newSpectator(GameId("gid"), "screenName1", false, PlayerAddress("address1"), TestClock)
      val spectator2 = newSpectator(GameId("gid"), "screenName2", false, PlayerAddress("address2"), TestClock)
      spectator1.playerId should not equal spectator2.playerId
    }

    "produces a different player key each time it is called" in {
      val spectator1 = newSpectator(GameId("gid"), "screenName1", false, PlayerAddress("address1"), TestClock)
      val spectator2 = newSpectator(GameId("gid"), "screenName2", false, PlayerAddress("address2"), TestClock)
      spectator1.playerKey should not equal spectator2.playerKey
    }
  }

  "updatePlayerAddress" - {
    "if the address has changed" - {
      "updates the player address as instructed" in {
        val newAddress = PlayerAddress("address-2")
        val player = newPlayer(GameId("gid"), "screenName", false, PlayerAddress("address"), TestClock)
        updatePlayerAddress(player, newAddress).value.playerAddress shouldEqual newAddress
      }

      "doesn't change anything else" in {
        val oldAddress = PlayerAddress("address")
        val newAddress = PlayerAddress("address-2")
        val player = newPlayer(GameId("gid"), "screenName", false, oldAddress, TestClock)
        val updatedWithOldAddress = updatePlayerAddress(player, newAddress).value
          .copy(playerAddress = oldAddress)
        updatedWithOldAddress shouldEqual player
      }
    }

    "if the address has not changed, returns None" in {
      val playerAddress = PlayerAddress("address")
      val player = newPlayer(GameId("gid"), "screenName", false, playerAddress, TestClock)
      val result = updatePlayerAddress(player, playerAddress)
      result shouldEqual None
    }
  }

  "addPlayerIds" - {
    val game = newGame("game name", false, TestClock, 123L)
    val gameDb = Representations.gameToDb(game)
    val player1 = newPlayer(game.gameId, "player name", false, PlayerAddress("address 1"), TestClock)
    val player1Db = Representations.playerToDb(player1)
    val player2 = newPlayer(game.gameId, "player 2 name", false, PlayerAddress("address 2"), TestClock)
    val player2Db = Representations.playerToDb(player2)
    val spectator1 = newSpectator(game.gameId, "spectator-1", false, PlayerAddress("spectator-address-1"), TestClock)
    val spectator1Db = Representations.spectatorToDb(spectator1)
    val spectator2 = newSpectator(game.gameId, "spectator-2", false, PlayerAddress("spectator-address-2"), TestClock)
    val spectator2Db = Representations.spectatorToDb(spectator2)

    "updates the game's players" - {
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

      "excludes spectators from player IDs list" in {
        addPlayerIds(gameDb, List(player1Db, spectator1Db)).playerIds shouldEqual List(player1Db.playerId)
      }
    }

    "updates the game's spectators" - {
      "includes a passed spectator's ID" in {
        addPlayerIds(gameDb, List(spectator1Db)).spectatorIds should contain(spectator1Db.playerId)
      }

      "includes multiple passed spectator's IDs" in {
        val spectatorIds = addPlayerIds(gameDb, List(spectator1Db, spectator2Db)).spectatorIds
        spectatorIds should contain.allOf(spectator1Db.playerId, spectator2Db.playerId)
      }

      "includes previous spectator IDs as well as new ones" in {
        val allSpectatorIds =
          addPlayerIds(
            addPlayerIds(gameDb, List(spectator1Db)),
            List(spectator2Db)
          ).spectatorIds
        allSpectatorIds should contain.allOf(spectator1Db.playerId, spectator2Db.playerId)
      }

      "excludes players from spectators IDs list" in {
        addPlayerIds(gameDb, List(player1Db, spectator1Db)).spectatorIds shouldEqual List(spectator1Db.playerId)
      }
    }
  }

  "addPlayer" - {
    val game = newGame("game name", false, TestClock, 123L)
    val player = newPlayer(game.gameId, "player name", false, PlayerAddress("address"), TestClock)

    "includes passed player in the game" in {
      addPlayer(game, player).players should contain(player)
    }

    "includes passed player as well as any existing players" in {
      val player2 = newPlayer(game.gameId, "player 2", false, PlayerAddress("address 2"), TestClock)
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

  "start" - {
    val game = newGame("game name", false, TestClock, 123L)

    "sets up some key fields" in {
      forAll { startTime: Long =>
        start(game, startTime, None, None, None, game.players.map(_.playerId)) should have(
          "started" as true,
          "startTime" as startTime,
          "button" as 0,
        )
      }
    }

    "trackStacks" - {
      "is true if initial stack levels are provided" in {
        forAll { startingStacks: Int =>
          start(game, 0L, None, None, Some(startingStacks), game.players.map(_.playerId)).trackStacks shouldEqual true
        }
      }

      "is false if no initial stack levels are provided" in {
        start(game, 0L, None, None, None, game.players.map(_.playerId)).trackStacks shouldEqual false
      }
    }

    "timer status" - {
      "is set if present" - {
        val timerLevels = Some(List(
          RoundLevel(10, 1),
          BreakLevel(10),
          RoundLevel(10, 2),
        ))
        "start time uses the 'now'" in {
          forAll { startTime: Long =>
            val gameTimer = start(game, startTime, None, timerLevels, None, game.players.map(_.playerId)).timer.value
            gameTimer.timerStartTime shouldEqual startTime
          }
        }

        "paused time is None" in {
          val gameTimer = start(game, 1000L, None, timerLevels, None, game.players.map(_.playerId)).timer.value
          gameTimer.pausedTime shouldEqual None
        }

        "uses the provided timer levels" in {
          val gameTimer = start(game, 1000L, None, timerLevels, None, game.players.map(_.playerId)).timer.value
          gameTimer.levels shouldEqual timerLevels.get
        }
      }

      "is empty if no timer is provided" in {
        start(game, 1000L, None, None, None, game.players.map(_.playerId)).timer shouldEqual None
      }
    }

    "sets up a round" - {
      "deals the same cards for the same seed" in {
        forAll { initialSeed: Long =>
          val seededGame = newGame("game name", false, TestClock, initialSeed)
          val round1 = start(seededGame, 1000L, None, None, None, game.players.map(_.playerId)).round
          val round2 = start(seededGame, 1000L, None, None, None, game.players.map(_.playerId)).round
          round1 should have(
            "burn1" as round2.burn1,
            "flop1" as round2.flop1,
            "flop2" as round2.flop2,
            "flop3" as round2.flop3,
            "burn2" as round2.burn2,
            "turn" as round2.turn,
            "burn3" as round2.burn3,
            "river" as round2.river,
          )
        }
      }

      "the round's phase starts at PreFlop" in {
        start(game, 1000L, None, None, None, game.players.map(_.playerId)).round.phase shouldEqual PreFlop
      }
    }

    "sets up the players properly" - {
      val players = List(
        newPlayer(game.gameId, "player-1", false, PlayerAddress("address-1"), TestClock),
        newPlayer(game.gameId, "player-2", false, PlayerAddress("address-2"), TestClock),
        newPlayer(game.gameId, "player-3", false, PlayerAddress("address-3"), TestClock),
        newPlayer(game.gameId, "player-4", false, PlayerAddress("address-4"), TestClock),
      )

      "the players are ordered using the provided player order" in {
        val playerIds = players.map(_.playerId)
        forAll { (shuffleSeed: Long) =>
          val playerOrder = new Random(shuffleSeed).shuffle(playerIds)
          val resultingPlayers = start(game.copy(seed = 1L, players = players), 1000L, None, None, None, playerOrder).players
          resultingPlayers.map(_.playerId) shouldEqual playerOrder
        }
      }

      "inTurn player" - {
        "activates third player in the provided player order, via the game's `inTurn` property (player after dealer and blinds)" in {
          val playerIds = players.map(_.playerId)
          forAll { (shuffleSeed: Long) =>
            val playerOrder = new Random(shuffleSeed).shuffle(playerIds)
            val result = start(game.copy(seed = 1L, players = players), 1000L, None, None, None, playerOrder)
            result.inTurn shouldEqual playerOrder.lift(3)
          }
        }

        "activates the dealer in a new heads-up game" in {
          val testPlayers = players.take(2)
          val playerIds = testPlayers.map(_.playerId)
          forAll { (shuffleSeed: Long) =>
            val playerOrder = new Random(shuffleSeed).shuffle(playerIds)
            val result = start(game.copy(seed = 1L, players = testPlayers), 1000L, None, None, None, playerOrder)
            result.inTurn shouldEqual playerOrder.headOption
          }
        }
      }


      "player holes are the same for the same seed" in {
        forAll { seed: Long =>
          val players1 = start(game.copy(seed = seed, players = players), 1000L, None, None, None, game.players.map(_.playerId)).players
          val players2 = start(game.copy(seed = seed, players = players), 1000L, None, None, None, game.players.map(_.playerId)).players
          players1.map(_.hole) shouldEqual players2.map(_.hole)
        }
      }

      "starting stacks" - {
        "are 0 if no starting stacks are specified" in {
          start(game.copy(players = players), 1000L, None, None, None, game.players.map(_.playerId)).players.map(_.stack) should contain only 0
        }

        "set to the specified amount, if present" in {
          forAll(Gen.choose(0, 1000)) { startingStack =>
            val result =
              start(game.copy(players = players), 1000L, None, None, Some(startingStack), game.players.map(_.playerId))
                .players.map(_.stack)
            result should contain only startingStack
          }
        }
      }

      "fixed blinds" - {
        "are paid out in a multi player game" in {
          val result = start(game.copy(players = players), 1000L, Some(5), None, Some(1000), players.map(_.playerId))
          result.players.map(_.bet).take(3) shouldEqual List(0, 5, 10)
        }

        "are paid out in a heads-up game (dealer is small blind)" in {
          val testPlayers = players.take(2)
          val result = start(game.copy(players = testPlayers), 1000L, Some(5), None, Some(1000), players.map(_.playerId))
          result.players.map(_.bet).take(3) shouldEqual List(5, 10)
        }

        "if big blind amount exceeds player stack the big blind player is put all-in" - {
          "their bet is limited to their entire stack" in {
            val result = start(game.copy(players = players), 1000L, Some(700), None, Some(1000), players.map(_.playerId))
            result.players.map(_.bet).take(3) shouldEqual List(0, 700, 1000)  // BB is 1000 stack size instead of expected 1400
          }

          "their stack is empty (and certainly not negative)" in {
            val result = start(game.copy(players = players), 1000L, Some(700), None, Some(1000), players.map(_.playerId))
            result.players.map(_.stack).apply(2) shouldEqual 0
          }
        }

        "if small blind amount exceeds player stack the small-blind player is put all-in" - {
          "their bet is limited to their entire stack" in {
            val result = start(game.copy(players = players), 1000L, Some(1200), None, Some(1000), players.map(_.playerId))
            result.players.map(_.bet).take(2) shouldEqual List(0, 1000)  // SB is 1000 stack size instead of expected 1200
          }

          "their stack is empty (and certainly not negative)" in {
            val result = start(game.copy(players = players), 1000L, Some(1200), None, Some(1000), players.map(_.playerId))
            result.players.map(_.stack)(1) shouldEqual 0
          }
        }
      }

      "timer-based blinds" - {
        "pays out according to first timer round's amount" in {
          val result = start(
            game.copy(players = players),
            1000L,
            None,
            Some(List(
              RoundLevel(300, 5),
              RoundLevel(300, 10),
              BreakLevel(60),
              RoundLevel(300, 20),
            )),
            Some(1000),
            players.map(_.playerId),
          )
          result.players.map(_.bet) shouldEqual List(0, 5, 10, 0)
        }

        "are paid out correctly in a heads-up game (dealer is small blind)" in {
          val testPlayers = players.take(2)
          val result = start(
            game.copy(players = testPlayers),
            1000L,
            None,
            Some(List(
              RoundLevel(300, 5),
              RoundLevel(300, 10),
              BreakLevel(60),
              RoundLevel(300, 20),
            )),
            Some(1000),
            testPlayers.map(_.playerId),
          )
          result.players.map(_.bet) shouldEqual List(5, 10)
        }
      }
    }
  }

  "updateBlindAction" - {
    val rawUpdateGame = UpdateBlind(
      GameId("game-id"), PlayerId("player-id"), PlayerKey("player-key"),
      None, None, None, None
    )

    "for a timer update" - {
      "returns update timer action" in {
        updateBlindAction(rawUpdateGame.copy(
          timerLevels = Some(List(RoundLevel(100, 10), BreakLevel(50)))
        )).value shouldEqual EditTimerSummary()
      }

      "action is edit timer even if the status also changes" in {
        updateBlindAction(rawUpdateGame.copy(
          timerLevels = Some(List(RoundLevel(100, 10), BreakLevel(50))),
          playing = Some(true),
        )).value shouldEqual EditTimerSummary()
      }

      "a progress update also counts as an edit timer action" in {
        updateBlindAction(rawUpdateGame.copy(
          progress = Some(10)
        )).value shouldEqual EditTimerSummary()
      }
    }

    "for a playing status update" - {
      "returns 'playing' timer status action if the timer was started" in {
        updateBlindAction(rawUpdateGame.copy(
          playing = Some(true),
        )).value shouldEqual TimerStatusSummary(true)
      }

      "returns 'paused' timer status action if the timer was stopped" in {
        updateBlindAction(rawUpdateGame.copy(
          playing = Some(false),
        )).value shouldEqual TimerStatusSummary(false)
      }
    }

    "returns update blind status for a manual blind edit" in {
      updateBlindAction(rawUpdateGame.copy(
        smallBlind = Some(10),
      )).value shouldEqual EditBlindSummary()
    }

    "returns an error if the request doesn't include any actions" in {
      updateBlindAction(rawUpdateGame).isLeft shouldEqual true
    }
  }

  "expiryTime" - {
    "expiry time is after the provided date" in {
      forAll { now: Long =>
        // let's not think ahead of the year 3000 to avoid Long overflow
        whenever(now < 32503680000000L) {
          expiryTime(now) should be > now
        }
      }
    }
  }

  "resetPlayerForNextPhase" - {
    val gameId = GameId("game-id")
    val player = newPlayer(gameId, "player", false, PlayerAddress("address"), TestClock)

    "unchecks a checked player" in {
      resetPlayerForNextPhase(player.copy(checked = true)).checked shouldEqual false
    }

    "does not unfold a folded player" in {
      resetPlayerForNextPhase(player.copy(folded = true)).folded shouldEqual true
    }

    "sets the player's bet value to 0" in {
      forAll { n: Int =>
        resetPlayerForNextPhase(player.copy(bet = n)).bet shouldEqual 0
      }
    }

    "sets the player's pot value to the sum of the previous pot value and their bet" in {
      forAll { (bet: Int, prevPot: Int) =>
        resetPlayerForNextPhase(
          player.copy(bet = bet, pot = prevPot)
        ).pot shouldEqual bet + prevPot
      }
    }
  }

  "resetPlayerForShowdown" - {
    val gameId = GameId("game-id")
    val player = newPlayer(gameId, "player", false, PlayerAddress("address"), TestClock)

    "updates the player's stack based on their winnings" in {
      forAll { (winnings: Int, previousStack: Int) =>
        val playerWinnings = List(
          PlayerWinnings(
            player.playerId,
            Some(HighCard(Ace of Clubs, King of Spades, Queen of Hearts, Ten of Diamonds, Eight of Clubs)),
            Hole(Ace of Clubs, King of Spades),
            winnings,
          )
        )
        resetPlayerForShowdown(playerWinnings)(player.copy(stack = previousStack)).stack shouldEqual (previousStack + winnings)
      }
    }

    "if the player does not have a winnings entry, their stack is left as it was" in {
      forAll { previousStack: Int =>
        resetPlayerForShowdown(Nil)(player.copy(stack = previousStack)).stack shouldEqual previousStack
      }
    }

    "forces player's checked state to true" in {
      forAll { checked: Boolean =>
        resetPlayerForShowdown(Nil)(player.copy(checked = checked)).checked shouldEqual true
      }
    }

    "does not unfold a folded player" in {
      resetPlayerForShowdown(Nil)(player.copy(folded = true)).folded shouldEqual true
    }
  }

  "resetPlayerForNextRound" - {
    val gameId = GameId("game-id")
    val player = newPlayer(gameId, "player", false, PlayerAddress("address"), TestClock)

    "zeroes the player's pot contribution" in {
      forAll { pot: Int =>
        resetPlayerForNextRound(player.copy(pot = pot)).pot shouldEqual 0
      }
    }

    "unfolds a folded player" in {
      resetPlayerForNextRound(player.copy(folded = true)).folded shouldEqual false
    }

    "unchecks a checked player" in {
      resetPlayerForNextRound(player.copy(checked = true)).checked shouldEqual false
    }

    "busts a player who is out of money" in {
      resetPlayerForNextRound(player.copy(stack = 0)).busted shouldEqual true
    }

    " not change the player's blind" in {
      forAll(Gen.oneOf(NoBlind, SmallBlind, BigBlind)) { blind =>
        resetPlayerForNextRound(player.copy(blind = blind)).blind shouldEqual blind
      }
    }
  }

  "resetPlayerForAbandonedRound" - {
    val gameId = GameId("game-id")
    val player = newPlayer(gameId, "player", false, PlayerAddress("address"), TestClock)

    "player's bet and pot are added to their stack" in {
      forAll(Gen.choose(0, 100), Gen.choose(0, 100), Gen.choose(0, 100)) { (stack, pot, bet) =>
        resetPlayerForAbandonedRound(
          player.copy(stack = stack, pot = pot, bet = bet)
        ).stack shouldEqual (stack + pot + bet)
      }
    }

    "zeroes the player's pot contribution" in {
      forAll(Gen.choose(0, 100)) { pot =>
        resetPlayerForAbandonedRound(
          player.copy(pot = pot)
        ).pot shouldEqual 0
      }
    }

    "zeroes the player's bet contribution" in {
      forAll(Gen.choose(0, 100)) { bet =>
        resetPlayerForAbandonedRound(
          player.copy(bet = bet)
        ).pot shouldEqual 0
      }
    }

    "unfolds a folded player" in {
      resetPlayerForAbandonedRound(player.copy(folded = true)).folded shouldEqual false
    }

    "unchecks a checked player" in {
      resetPlayerForAbandonedRound(player.copy(checked = true)).checked shouldEqual false
    }

    "does not change the player's blind" in {
      forAll(Gen.oneOf(NoBlind, SmallBlind, BigBlind)) { blind =>
        resetPlayerForAbandonedRound(player.copy(blind = blind)).blind shouldEqual blind
      }
    }
  }

  "requireGame" - {
    "returns the gameDb if present" in {
      val gameDb = Representations.gameToDb(
        newGame("game name", false, TestClock, 123L)
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
      val game = newGame("game name", false, TestClock, 123L)
      ensureNotStarted(
        game.copy(started = false)
      ).isRight shouldEqual true
    }

    "fails if the game has already started" in {
      val game = newGame("game name", false, TestClock, 123L)
      ensureNotStarted(
        game.copy(started = true)
      ).isLeft shouldEqual true
    }
  }

  "ensureStarted" - {
    val game = newGame("Game name", false, TestClock, 123L)

    "is successful if the game has begun" in {
      val started = game.copy(started = true)
      ensureStarted(started).value shouldEqual ()
    }

    "fails if the game has not begun" in {
      ensureStarted(game).isLeft shouldEqual true
    }
  }

  "ensureNoDuplicateScreenName" - {
    val game = newGame("game name", false, TestClock, 123L)

    "is fine for a game with no players" in {
      forAll { screenName: String =>
        ensureNoDuplicateScreenName(game, screenName).isRight shouldEqual true
      }
    }

    "is successful for a screen name that isn't already taken" in {
      forAll { screenName: String =>
        whenever(screenName != "screenname") {
          val player = newPlayer(game.gameId, "screenname", false, PlayerAddress("address"), TestClock)
          val gameWithPlayer = addPlayer(game, player)
          ensureNoDuplicateScreenName(gameWithPlayer, screenName).isRight shouldEqual true
        }
      }
    }

    "fails for a screen name that is already in use in this game" in {
      forAll { screenName: String =>
        val player = newPlayer(game.gameId, screenName, false, PlayerAddress("address"), TestClock)
        val gameWithPlayer = addPlayer(game, player)
        ensureNoDuplicateScreenName(gameWithPlayer, screenName).isLeft shouldEqual true
      }
    }
  }

  "ensurePlayerCount" - {
    "succeeds if there are a sensible number of players" in {
      forAll(Gen.choose(0, 19)) { n =>
        ensurePlayerCount(n).value shouldEqual ()
      }
    }

    "fails when there are already 20 players" in {
      ensurePlayerCount(20).isLeft shouldEqual true
    }

    "fails if there are somehow already more than 20 players" in {
      forAll(Gen.choose(21, 50)) { n =>
        ensurePlayerCount(n).isLeft shouldEqual true
      }
    }
  }

  "ensureStartingPlayerCount" - {
    "succeeds if the player count is more than 1" in {
      forAll(Gen.choose(2, 20)) { n =>
        ensureStartingPlayerCount(n).value shouldEqual ()
      }
    }

    "fails if ther is only one player in the game" in {
      ensureStartingPlayerCount(1).isLeft shouldEqual true
    }
  }

  "ensureNotAlreadyPlaying" - {
    val game = newGame("game name", false, TestClock, 123L)

    "succeeds for a player address that isn't already in use" in {
      forAll { address: String =>
        whenever(address != "address1") {
          val player = newPlayer(game.gameId, "screenname", false, PlayerAddress("address1"), TestClock)
          val gameWithPlayer = addPlayer(game, player)
          ensureNotAlreadyPlaying(gameWithPlayer.players, PlayerAddress(address)).isRight shouldEqual true
        }
      }
    }

    "fails for a player address that is already being used" in {
      forAll { address: String =>
        val player = newPlayer(game.gameId, "screenname", false, PlayerAddress(address), TestClock)
        val gameWithPlayer = addPlayer(game, player)
        ensureNotAlreadyPlaying(gameWithPlayer.players, PlayerAddress(address)).isLeft shouldEqual true
      }
    }
  }

  "ensurePlayerKey" - {
    val game = newGame("game name", false, TestClock, 123L)
    val player = newPlayer(game.gameId, "player name", false, PlayerAddress("address"), TestClock)

    "if the player is part of this game" - {
      val gameWithPlayer = addPlayer(game, player)

      "returns the valid player if their key is valid" in {
        val playerResult = ensurePlayerKey(gameWithPlayer.players, player.playerId, player.playerKey).value
        playerResult shouldEqual player
      }

      "fails if the player key does not match" in {
        val incorrectPlayerKey = PlayerKey("bad player key")
        val result = ensurePlayerKey(gameWithPlayer.players, player.playerId, incorrectPlayerKey)
        result.isLeft shouldEqual true
      }
    }

    "fails if the player does not exist in the game" in {
      val result = ensurePlayerKey(game.players, player.playerId, player.playerKey)
      result.isLeft shouldEqual true
    }
  }

  "ensureSpectatorKey" - {
    val game = newGame("game name", false, TestClock, 123L)
    val spectator = newSpectator(game.gameId, "player name", false, PlayerAddress("address"), TestClock)

    "if the spectator is part of this game" - {
      val gameWithPlayer = addSpectator(game, spectator)

      "returns the valid spectator if their key is valid" in {
        val spectatorResult = ensureSpectatorKey(gameWithPlayer.spectators, spectator.playerId, spectator.playerKey).value
        spectatorResult shouldEqual spectator
      }

      "fails if the spectator's player key does not match" in {
        val incorrectPlayerKey = PlayerKey("bad player key")
        val result = ensureSpectatorKey(gameWithPlayer.spectators, spectator.playerId, incorrectPlayerKey)
        result.isLeft shouldEqual true
      }
    }

    "fails if the spectator does not exist in the game" in {
      val result = ensureSpectatorKey(game.spectators, spectator.playerId, spectator.playerKey)
      result.isLeft shouldEqual true
    }
  }

  "ensureHost" - {
    val gameId = GameId("game-id")
    val host = newPlayer(gameId, "host", true, PlayerAddress("host-address"), TestClock)
    val player1 = newPlayer(gameId, "player-1", false, PlayerAddress("player-1-address"), TestClock)
    val players = List(host, player1)

    "succeeds if the provided player is the host" in {
      ensureHost(players, host.playerKey).value shouldEqual host
    }

    "fails if the provided player is not the host" in {
      ensureHost(players, player1.playerKey).isLeft shouldEqual true
    }

    "fails if the provided player does not exist in the game" in {
      val nonPlayerKey = PlayerKey("not-in-the-game")
      ensureHost(players, nonPlayerKey).isLeft shouldEqual true
    }
  }

  "ensureAdmin" - {
    val gameId = GameId("game-id")
    val admin = newPlayer(gameId, "host", true, PlayerAddress("host-address"), TestClock)
    val player1 = newPlayer(gameId, "player-1", false, PlayerAddress("player-1-address"), TestClock)
    val players = List(admin, player1)

    "succeeds if the provided player is the host" in {
      ensureAdmin(players, admin.playerKey).value shouldEqual admin
    }

    "fails if the provided player is not the host" in {
      ensureAdmin(players, player1.playerKey).isLeft shouldEqual true
    }

    "fails if the provided player does not exist in the game" in {
      val nonPlayerKey = PlayerKey("not-in-the-game")
      ensureAdmin(players, nonPlayerKey).isLeft shouldEqual true
    }
  }

  "ensureActive" - {
    "succeeds if the player is active" in {
      val pid = PlayerId("player-id")
      ensureActive(Some(pid), pid).isRight shouldEqual true
    }

    "fails if the player is not active" in {
      ensureActive(
        Some(PlayerId("active-player-id")),
        PlayerId("different-player-id")
      ).isLeft shouldEqual true
    }

    "fails if no player is active" in {
      ensureActive(None, PlayerId("player")).isLeft shouldEqual true
    }
  }
}
