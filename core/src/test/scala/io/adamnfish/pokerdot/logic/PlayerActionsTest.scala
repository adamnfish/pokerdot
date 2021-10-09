package io.adamnfish.pokerdot.logic

import io.adamnfish.pokerdot.logic.Cards.RichRank
import io.adamnfish.pokerdot.logic.Games.{newGame, newPlayer}
import io.adamnfish.pokerdot.logic.PlayerActions._
import io.adamnfish.pokerdot.models._
import io.adamnfish.pokerdot.{ConfigurableTestClock, TestClock, TestHelpers, TestRng}
import org.scalacheck.Gen
import org.scalatest.OptionValues
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks


class PlayerActionsTest extends AnyFreeSpec with Matchers with TestHelpers with ScalaCheckDrivenPropertyChecks with OptionValues {
  "bet" - {
    "reduces player's stack by the bet amount" ignore {}
    "increases player's bet by the bet amount" ignore {}

    "for a raise" - {
      "checks this player" ignore {}
      "unchecks all other players" ignore {}
    }

    "for a call" - {
      "checks this player" ignore {}
      "does not uncheck other players" ignore {}
    }

    "activates the next eligible player" ignore {}

    "returns a bet action" - {
      "with correct player" ignore {}
      "with correct bet amount" ignore {}
    }

    "fails if the bet is not large enough to call other players' bets" ignore {}
    "fails if this raise is less than the big blind" ignore {}
    "fails if this raise is not as large as a previous raise" ignore {}
    "fails if it is not the player's turn" ignore {} // this is checked in the controller instead
    "fails if the bet exceeds the player's stack" ignore {}
  }

  "check" - {
    "updates the player's checked status" ignore {}
    "activates the next eligible player" ignore {}

    "returns a check action with correct player" ignore {}

    "fails if it is not the player's turn" ignore {} // this is checked in the controller instead
    "fails if the player has not called the current highest bet" ignore {}
    "fails if the player has already checked" ignore {}
  }

  "fold" - {
    val rawGame = newGame("Game name", trackStacks = true, TestClock, 1L)
    val p1 = newPlayer(rawGame.gameId, "p1", isHost = false, PlayerAddress("p1-address"), TestClock)
      .copy(stack = 1000)
    val p2 = newPlayer(rawGame.gameId, "p2", isHost = false, PlayerAddress("p2-address"), TestClock)
      .copy(stack = 1000)
    val p3 = newPlayer(rawGame.gameId, "p3", isHost = false, PlayerAddress("p3-address"), TestClock)
      .copy(stack = 1000)

    "updates player's folded status" in {
      val game = rawGame.copy(
        inTurn = Some(p1.playerId),
        players = List(
          p1,
          p2.copy(blind = SmallBlind, bet = 5),
          p3.copy(blind = BigBlind, bet = 10),
        )
      )
      val updatedGame = fold(game, p1)
      val updatedP1 = updatedGame.players.find(_.playerId == p1.playerId).value
      updatedP1.folded shouldEqual true
    }

    "activates the next eligible player" in {
      val game = rawGame.copy(
        inTurn = Some(p1.playerId),
        players = List(
          p1,
          p2.copy(blind = SmallBlind, bet = 5),
          p3.copy(blind = BigBlind, bet = 10),
        )
      )
      val updatedGame = fold(game, p1)
      updatedGame.inTurn shouldEqual Some(p2.playerId)
    }

    "no one is 'in turn' if there is only one player remaining" - {
      "heads up fold" in {
        val game = rawGame.copy(
          inTurn = Some(p1.playerId),
          players = List(
            p1.copy(blind = SmallBlind, bet = 5),
            p2.copy(blind = BigBlind, bet = 10),
          )
        )
        val updatedGame = fold(game, p1)
        updatedGame.inTurn shouldEqual None
      }

      "busted players leaves heads-up" in {
        val game = rawGame.copy(
          inTurn = Some(p1.playerId),
          players = List(
            p1.copy(blind = SmallBlind, bet = 5),
            p2.copy(blind = BigBlind, bet = 10),
            p3.copy(busted = true),
          )
        )
        val updatedGame = fold(game, p1)
        updatedGame.inTurn shouldEqual None
      }

      "other players have already folded so this fold leaves a single winner" in {
        val game = rawGame.copy(
          inTurn = Some(p2.playerId),
          players = List(
            p1.copy(folded = true),
            p2.copy(blind = SmallBlind, bet = 5),
            p3.copy(blind = BigBlind, bet = 10),
          )
        )
        val updatedGame = fold(game, p2)
        updatedGame.inTurn shouldEqual None
      }
    }

    "returns a fold action with correct player" ignore {}

    "fails if it is not the player's turn" ignore {} // this is checked in the controller instead
    "fails if the player has already folded" ignore {}
  }

  "advancePhase" - {
    "for the simple phases" - {
      val game = newGame("Game name", trackStacks = true, TestClock, 1L)
      val p1 :: p2 :: p3 :: p4 :: Nil = Play.dealHoles(
        List(
          newPlayer(game.gameId, "p1", isHost = false, PlayerAddress("p1-address"), TestClock)
            .copy(stack = 1000),
            newPlayer(game.gameId, "p2", isHost = false, PlayerAddress("p2-address"), TestClock)
            .copy(stack = 1000),
            newPlayer(game.gameId, "p3", isHost = false, PlayerAddress("p3-address"), TestClock)
            .copy(stack = 1000),
            newPlayer(game.gameId, "p4", isHost = false, PlayerAddress("p4-address"), TestClock)
            .copy(stack = 1000),
        ),
        Play.deckOrder(game.seed),
      )

      "game phase is advanced" in {
        val expected = Map(
          PreFlop -> Flop,
          Flop -> Turn,
          Turn -> River,
        )
        forAll(Gen.oneOf(PreFlop, Flop, Turn)) { phase =>
          val (newGame, _, _) = advancePhase(
            game.copy(
              round = game.round.copy(phase = phase),
            ), TestClock, TestRng
          ).value
          val nextPhase = newGame.round.phase
          nextPhase shouldEqual expected.get(phase).value
        }
      }

      "copies bet amounts to their pots" in {
        forAll(Gen.oneOf(PreFlop, Flop, Turn)) { phase =>
          val testGame = game.copy(
            round = game.round.copy(phase = phase),
            players = List(
              p1.copy(
                bet = 25,
                checked = true,
              ),
              p2.copy(
                bet = 5,
                folded = true,
              ),
              p3.copy(
                bet = 25,
                checked = true,
              ),
              p4.copy(
                bet = 25,
                checked = true,
              ),
            )
          )
          val (newGame, _, _) = advancePhase(testGame, TestClock, TestRng).value
          newGame.players.map(_.pot) shouldEqual List(
            25, 5, 25, 25
          )
        }
      }

      "zeroes player bet amounts" in {
        forAll(Gen.oneOf(PreFlop, Flop, Turn)) { phase =>
          val testGame = game.copy(
            round = game.round.copy(phase = phase),
            players = List(
              p1.copy(
                bet = 25,
                checked = true,
              ),
              p2.copy(
                bet = 5,
                folded = true,
              ),
              p3.copy(
                bet = 25,
                checked = true,
              ),
              p4.copy(
                bet = 25,
                checked = true,
              ),
            )
          )
          val (newGame, _, _) = advancePhase(testGame, TestClock, TestRng).value
          newGame.players.map(_.bet) shouldEqual List(
            0, 0, 0, 0
          )
        }
      }

      "unchecks players" in {
        forAll(Gen.oneOf(PreFlop, Flop, Turn)) { phase =>
          val testGame = game.copy(
            round = game.round.copy(phase = phase),
            players = List(
              p1.copy(
                bet = 25,
                checked = true,
              ),
              p2.copy(
                bet = 5,
                checked = false,
                folded = true,
              ),
              p3.copy(
                bet = 25,
                checked = true,
              ),
              p4.copy(
                bet = 25,
                checked = true,
              ),
            )
          )
          val (newGame, _, _) = advancePhase(testGame, TestClock, TestRng).value
          newGame.players.map(_.checked) shouldEqual List(
            false, false, false, false
          )
        }
      }

      "preserves fold statuses" in {
        forAll(Gen.oneOf(PreFlop, Flop, Turn)) { phase =>
          val testGame = game.copy(
            round = game.round.copy(phase = phase),
            players = List(
              p1.copy(
                bet = 25,
                checked = true,
              ),
              p2.copy(
                bet = 5,
                checked = false,
                folded = true, // this player has folded
              ),
              p3.copy(
                bet = 25,
                checked = true,
              ),
              p4.copy(
                bet = 25,
                checked = true,
              ),
            )
          )
          val (newGame, _, _) = advancePhase(testGame, TestClock, TestRng).value
          newGame.players.map(_.folded) shouldEqual List(
            false, true, false, false
          )
        }
      }

      "preserves busted status" in {
        forAll(Gen.oneOf(PreFlop, Flop, Turn)) { phase =>
          val testGame = game.copy(
            round = game.round.copy(phase = phase),
            players = List(
              p1.copy(
                bet = 25,
                checked = true,
              ),
              p2.copy(
                bet = 0,
                busted = true, // this player is busted
              ),
              p3.copy(
                bet = 25,
                checked = true,
              ),
              p4.copy(
                bet = 25,
                checked = true,
              ),
            )
          )
          val (newGame, _, _) = advancePhase(testGame, TestClock, TestRng).value
          newGame.players.map(_.busted) shouldEqual List(
            false, true, false, false
          )
        }
      }

      "fails if a player is yet to act" in {
        forAll(Gen.oneOf(PreFlop, Flop, Turn)) { phase =>
          val testGame = game.copy(
            round = game.round.copy(phase = phase),
            players = List(
              p1.copy(
                bet = 25,
                checked = true,
              ),
              p2.copy(
                bet = 25,
                checked = true,
              ),
              p3.copy(
                bet = 25,
                checked = true,
              ),
              p4.copy(
                bet = 25,
                checked = false, // player is yet to act
              ),
            )
          )
          advancePhase(testGame, TestClock, TestRng).isLeft shouldEqual true
        }
      }

      "resets active player" - {
        "active player is left of the dealer" in {
          forAll(Gen.oneOf(PreFlop, Flop, Turn)) { phase =>
            val testGame = game.copy(
              inTurn = Some(p1.playerId),
              round = game.round.copy(phase = phase),
              button = 0,
              players = List(
                p1.copy(
                  bet = 25,
                  checked = true,
                ),
                p2.copy(
                  bet = 25,
                  checked = true,
                ),
                p3.copy(
                  bet = 25,
                  checked = true,
                ),
                p4.copy(
                  bet = 25,
                  checked = true,
                ),
              )
            )
            val (newGame, _, _) = advancePhase(testGame, TestClock, TestRng).value
            newGame.inTurn shouldEqual Some(p2.playerId)
          }
        }

        "active player skips ineligible players" in {
          forAll(Gen.oneOf(PreFlop, Flop, Turn)) { phase =>
            val testGame = game.copy(
              inTurn = Some(p1.playerId),
              round = game.round.copy(phase = phase),
              button = 0,
              players = List(
                p1.copy(
                  bet = 25,
                  checked = true,
                ),
                p2.copy(
                  folded = true,
                ),
                p3.copy(
                  bet = 25,
                  checked = true,
                ),
                p4.copy(
                  bet = 25,
                  checked = true,
                ),
              )
            )
            val (newGame, _, _) = advancePhase(testGame, TestClock, TestRng).value
            newGame.inTurn shouldEqual Some(p3.playerId)
          }
        }
      }

      "skips straight to showdown if only one player remains in the round" in {
        forAll(Gen.oneOf(PreFlop, Flop, Turn)) { phase =>
          val round = Play.generateRound(phase, 5, game.seed)
          val testGame = game.copy(
            round = round,
            players = List(
              p1.copy(
                folded = true,
                bet = 25,
                pot = 10,
                blind = BigBlind,
              ),
              p2.copy(
                bet = 35,
                pot = 10
              ),
              p3.copy(
                folded = true,
                bet = 10,
                pot = 10,
                blind = SmallBlind,
              ),
            )
          )
          val (updatedGame, _, _) = advancePhase(testGame, TestClock, TestRng).value
          updatedGame.round.phase shouldEqual Showdown
        }
      }
    }

    "when advancing from the river phase to the showdown" - {
      "calculates winning hands and amounts" ignore {}
      "updates player stacks based on their winnings" ignore {}
      "updates pots with final bet amounts" ignore {}
    }

    "when advancing from the showdown to a new round" - {
      "empties player pots" ignore {}
      "busts players that have run out of money" ignore {}

      "shuffles the deck" - {
        "deals new player holes" ignore {}
        "deals new cards for the round" ignore {}
      }

      "calculates the positions for the next round" - {
        "for a typical case" - {
          "the button moves ahead one" ignore {}
          "the small blind moves ahead one" ignore {}
          "the big blind moves ahead one" ignore {}
        }

        "big blind skips a non-eligible player" ignore {}
        "small blind skips a non-eligible player" ignore {}
        "button skips a non-eligible player" ignore {}

        "if only the big blind was present" - {
          "dealer stays" ignore {}
          "previous big blind is now small blind" ignore {}
          "next big blind as normal" ignore {}
        }

        "if players are eliminated in this round it can be more complex" - {
          "if big and small blind were both present this round" - {
            "if the current small blind is busted" - {
              "dealer stays" ignore {}
              "previous big blind is now small blind" ignore {}
              "next big blind is as normal" ignore {}
            }

            "if the current big blind is busted" - {
              "dealer moves" ignore {}
              "no small blind" ignore {}
              "next big blind as normal" ignore {}
            }

            "if both current blinds are busted" - {
              "dealer stays" ignore {}
              "no small blind" ignore {}
              "next big blind as normal" ignore {}
            }

            "if both current blinds and the dealer are busted" - {
              "dealer moves back one" ignore {}
              "no small blind" ignore {}
              "next big blind as normal" ignore {}
            }
          }

          "if only the big blind was present" - {
            "if the current big blind is busted" - {
              "dealer stays" ignore {}
              "no small blind again" ignore {}
              "next big blind as normal" ignore {}
            }

            "if both the current big blind and the dealer is busted" - {
              "dealer moves back one" ignore {}
              "no small blind again" ignore {}
              "next big blind as normal" ignore {}
            }
          }
        }
      }
    }
  }

  "advanceFromRiver" - {
    val rawGame = newGame("Game name", trackStacks = true, TestClock, 1L)
    val p1 = newPlayer(rawGame.gameId, "p1", false, PlayerAddress("p1-address"), TestClock)
    val p2 = newPlayer(rawGame.gameId, "p2", false, PlayerAddress("p2-address"), TestClock)
    val p3 = newPlayer(rawGame.gameId, "p3", false, PlayerAddress("p3-address"), TestClock)
    val round = Play.generateRound(River, 5, rawGame.seed)

    "excludes folded players from the player hands" in {
      val dealtPlayers = Play.dealHoles(
        List(
          p1.copy(
            pot = 10,
          ),
          p2.copy(
            pot = 10,
          ),
          p3.copy(
            pot = 5,
            folded = true,
          ),
        ),
        Play.deckOrder(rawGame.seed)
      )
      val game = rawGame.copy(
        round = round,
        players = dealtPlayers,
      )
      val (_, playerWinnings, _) = advanceFromRiver(game)
      playerWinnings.find(_.playerId == p3.playerId) shouldEqual None
    }
  }

  "advanceFromFoldedFinish" - {
    val rawGame = newGame("Game name", trackStacks = true, TestClock, 1L)
    val p1 :: p2 :: p3 :: Nil = Play.dealHoles(
      List(
        newPlayer(rawGame.gameId, "p1", false, PlayerAddress("p1-address"), TestClock),
        newPlayer(rawGame.gameId, "p2", false, PlayerAddress("p2-address"), TestClock),
        newPlayer(rawGame.gameId, "p3", false, PlayerAddress("p3-address"), TestClock),
      ),
      Play.deckOrder(rawGame.seed),
    )

    "is correct for an example, heads-up" in {
      forAll(Gen.oneOf(PreFlop, Flop, Turn)) { phase =>
        val round = Play.generateRound(phase, 5, rawGame.seed)
        val game = rawGame.copy(
          round = round,
          players = List(
            p1.copy(folded = true, bet = 25, pot = 10),
            p2.copy(bet = 35, pot = 10),
          )
        )
        val (_, playerWinnings, potWinnings) = advanceFromFoldedFinish(game)
        val expectedHaul = 25 + 10 + 35 + 10
        playerWinnings shouldEqual PlayerWinnings(p2.playerId, None, p2.hole.get, expectedHaul)
        potWinnings shouldEqual PotWinnings(expectedHaul, Set(p2.playerId), Set(p2.playerId))
      }
    }

    "is correct for an example, larger game" in {
      forAll(Gen.oneOf(PreFlop, Flop, Turn)) { phase =>
        val round = Play.generateRound(phase, 5, rawGame.seed)
        val game = rawGame.copy(
          round = round,
          players = List(
            p1.copy(folded = true, bet = 25, pot = 10),
            p2.copy(bet = 35, pot = 10),
            p3.copy(folded = true, bet = 10, pot = 10),
          )
        )
        val (_, playerWinnings, potWinnings) = advanceFromFoldedFinish(game)
        val expectedHaul = 25 + 10 + 35 + 10 + 10 + 10
        playerWinnings shouldEqual PlayerWinnings(p2.playerId, None, p2.hole.get, expectedHaul)
        potWinnings shouldEqual PotWinnings(expectedHaul, Set(p2.playerId), Set(p2.playerId))
      }
    }
  }

  "startNewRound" - {
    val rawGame = newGame("Game name", trackStacks = true, TestClock, 1L)
    val p1 :: p2 :: p3 :: Nil = Play.dealHoles(
      List(
        newPlayer(rawGame.gameId, "p1", false, PlayerAddress("p1-address"), TestClock),
        newPlayer(rawGame.gameId, "p2", false, PlayerAddress("p2-address"), TestClock),
        newPlayer(rawGame.gameId, "p3", false, PlayerAddress("p3-address"), TestClock),
      ),
      Play.deckOrder(rawGame.seed),
    )

    "advances to a new PreFlop round" - {
      val round = Play.generateRound(Showdown, 5, rawGame.seed)
      val game = rawGame.copy(
        round = round,
        players = List(
          p1.copy(pot = 200, stack = 800),
          p2.copy(pot = 200, stack = 20, blind = BigBlind),
          p3.copy(stack = 0, busted = true)
        )
      )

      "round is now PreFlop" in {
        val resultingGame = startNewRound(game, TestClock, TestRng).value
        resultingGame.round.phase shouldEqual PreFlop
      }

      "deals new holes to all players non-busted" in {
        val resultingGame = startNewRound(game, TestClock, TestRng).value
        resultingGame.players.map(_.hole.isDefined) shouldEqual List(true, true, false)
      }

      "new cards are dealt" in {
        val resultingGame = startNewRound(game, TestClock, TestRng).value
        resultingGame.seed should not equal game.seed
      }

      "all players have empty pots after new round starts" in {
        val resultingGame = startNewRound(game, TestClock, TestRng).value
        resultingGame.players.map(_.pot) shouldEqual List(0, 0, 0)
      }

      "in a timer game, updates the blinds if the timer level has progressed" in {
        val round = Play.generateRound(Showdown, 10, rawGame.seed)
        val game = rawGame.copy(
          round = round,
          players = List(
            p1.copy(pot = 200, stack = 800),
            p2.copy(pot = 200, stack = 20, blind = BigBlind),
            p3.copy(stack = 0, busted = true)
          ),
          timer = Some(
            TimerStatus(0, None, List(
              RoundLevel(100, 10),
              RoundLevel(100, 20),
              BreakLevel(100),
              RoundLevel(100, 50),
            ))
          ),
        )
        val updatedGame = startNewRound(game, new ConfigurableTestClock(150 * 1000), TestRng).value
        updatedGame.round.smallBlind shouldEqual 20
      }
    }

    "fails if the game is over (fewer than two players are not busted after resolving the round)" in {
      val round = Play.generateRound(Showdown, 5, rawGame.seed)
      val game = rawGame.copy(
        round = round,
        players = List(
          p1.copy(pot = 200, stack = 0),  // this player is about to bust because their stack is empty
          p2.copy(pot = 200, stack = 20, blind = BigBlind),
          p3.copy(stack = 0, busted = true)
        )
      )
      startNewRound(game, TestClock, TestRng).isLeft shouldEqual true
    }

    "fails if there is a paused timer" in {
      val round = Play.generateRound(Showdown, 5, rawGame.seed)
      val game = rawGame.copy(
        round = round,
        players = List(
          p1.copy(pot = 200, stack = 800),
          p2.copy(pot = 200, stack = 20, blind = BigBlind),
          p3.copy(stack = 0, busted = true)
        ),
        timer = Some(
          TimerStatus(0, Some(80), List(
            RoundLevel(100, 10),
            RoundLevel(100, 20),
            BreakLevel(100),
            RoundLevel(100, 50),
          ))
        ),
      )
      val result = startNewRound(game, new ConfigurableTestClock(250), TestRng)
      result.isLeft shouldEqual true
    }
  }

  "updateBlind" - {
    val rawUpdateGame = UpdateBlind(
      GameId("game-id"), PlayerId("player-id"), PlayerKey("player-key"),
      None, None, None
    )
    val rawGame = newGame("Game name", trackStacks = true, TestClock, 1L)
    val p1 = newPlayer(rawGame.gameId, "player 1", isHost = false, PlayerAddress("p1-address"), TestClock)
    val p3 = newPlayer(rawGame.gameId, "player 2", isHost = false, PlayerAddress("p2-address"), TestClock)
    val p2 = newPlayer(rawGame.gameId, "player 3", isHost = false, PlayerAddress("p3-address"), TestClock)
    val game = rawGame.copy(
      players = List(p1, p2, p3),
      started = true,
    )

    "for a timer update" - {
      "updates the timer levels" in {
        val updatedGame = updateBlind(game,
          rawUpdateGame.copy(
            timerLevels = Some(List(RoundLevel(100, 10), BreakLevel(50)))
          ),
          now = 1000L
        )
        val timerStatus = updatedGame.value.timer.value
        timerStatus.levels shouldEqual List(RoundLevel(100, 10), BreakLevel(50))
      }

      "updates the timer status" in {
        val updatedGame = updateBlind(game,
          rawUpdateGame.copy(
            timerLevels = Some(List(RoundLevel(100, 10), BreakLevel(50)))
          ),
          now = 0L
        )
        updatedGame.value.timer.value should have(
          "timerStartTime" as 0L,
          "pausedTime" as None,
        )
      }
    }

    "for a playing status update" - {
      "for a pause request" - {
        "pauses the timer when playing is false" in {
          val updatedGame = updateBlind(
            game.copy(
              timer = Some(
                TimerStatus(
                  timerStartTime = 0L,
                  pausedTime = None,
                  levels = List(
                    RoundLevel(1500, 10),
                    BreakLevel(10),
                  )
                )
              )
            ),
            rawUpdateGame.copy(
              playing = Some(false),
            ),
            now = 1000L
          )
          val timerStatus = updatedGame.value.timer.value
          timerStatus.pausedTime shouldEqual Some(1000L)
        }

        "fails to pause game timer if it was already paused" in {
          updateBlind(
            game.copy(
              timer = Some(
                TimerStatus(
                  timerStartTime = 0L,
                  pausedTime = Some(100L),
                  levels = List(RoundLevel(100, 10), BreakLevel(50))
                )
              )
            ),
            rawUpdateGame.copy(
              playing = Some(false),
            ),
            now = 1000L
          ).isLeft shouldEqual true
        }
      }

      "for a timer restart" - {
        // TODO: test and implement pausing / restarting
        "calculates a correct start time from how long has elapsed" in {
          val updatedGame = updateBlind(
            game.copy(
              timer = Some(
                TimerStatus(
                  timerStartTime = 0L,
                  pausedTime = Some(100L),
                  levels = List(RoundLevel(100, 10), BreakLevel(50))
                )
              )
            ),
            rawUpdateGame.copy(
              playing = Some(true),
            ),
            now = 1000L
          )
          val timerStatus = updatedGame.value.timer.value
          timerStatus.timerStartTime shouldEqual 900L
        }

        "restarts the timer" in {
          val updatedGame = updateBlind(
            game.copy(
              timer = Some(
                TimerStatus(
                  timerStartTime = 0L,
                  pausedTime = Some(100L),
                  levels = List(RoundLevel(100, 10), BreakLevel(50))
                )
              )
            ),
            rawUpdateGame.copy(
              playing = Some(true),
            ),
            now = 1000L
          )
          val timerStatus = updatedGame.value.timer.value
          timerStatus.pausedTime shouldEqual None
        }

        "fails to restart the game timer if the it was already running" in {
          updateBlind(
            game.copy(
              timer = Some(
                TimerStatus(
                  timerStartTime = 0L,
                  pausedTime = None,
                  levels = List(RoundLevel(100, 10), BreakLevel(50))
                )
              )
            ),
            rawUpdateGame.copy(
              playing = Some(true),
            ),
            now = 1000L
          ).isLeft shouldEqual true
        }
      }
    }

    "for a manual blind update" - {
      "sets the blind to the specified amount" in {
        val updatedGame = updateBlind(
          game.copy(
            round = Round(
              River, 10, Two of Clubs, Three of Diamonds, Four of Spades, Five of Hearts, Six of Clubs, Seven of Diamonds, Eight of Hearts, Nine of Spades
            )
          ),
          rawUpdateGame.copy(
            smallBlind = Some(20),
          ),
          now = 1000L
        )
        updatedGame.value.round.smallBlind shouldEqual 20
      }

      "removes any existing timer" in {
        val updatedGame = updateBlind(
          game.copy(
            round = Round(
              River, 10, Two of Clubs, Three of Diamonds, Four of Spades, Five of Hearts, Six of Clubs, Seven of Diamonds, Eight of Hearts, Nine of Spades
            )
          ),
          rawUpdateGame.copy(
            smallBlind = Some(20),
          ),
          now = 1000L
        )
        updatedGame.value.timer shouldEqual None
      }
    }
  }

  "ensurePlayersHaveFinishedActing" - {
    val game = newGame("Game name", trackStacks = true, TestClock, 1L)
    val p1 = newPlayer(game.gameId, "player 1", isHost = false, PlayerAddress("p1-address"), TestClock).copy(
      stack = 1000
    )
    val p3 = newPlayer(game.gameId, "player 2", isHost = false, PlayerAddress("p2-address"), TestClock).copy(
      stack = 1000
    )
    val p2 = newPlayer(game.gameId, "player 3", isHost = false, PlayerAddress("p3-address"), TestClock).copy(
      stack = 1000
    )

    "if there are no players yet to act" - {
      "succeeds when all players have folded" in {
        ensurePlayersHaveFinishedActing(
          game.copy(
            players = List(p1, p2, p3).map(_.copy(folded = true))
          )
        ).isRight shouldEqual true
      }

      "succeeds when all players have checked at the same amount" in {
        ensurePlayersHaveFinishedActing(
          game.copy(
            players = List(p1, p2, p3).map(_.copy(
              bet = 50,
              checked = true,
            ))
          )
        ).isRight shouldEqual true
      }

      "succeeds if a player has not acted because they are all-in" in {
        ensurePlayersHaveFinishedActing(
          game.copy(
            players = List(
              p1.copy(
                bet = 50,
                checked = true,
              ),
              p2.copy(
                bet = 50,
                checked = true,
              ),
              p3.copy(
                bet = 20,
                stack = 0,
                checked = false,
              ),
            )
          )
        ).isRight shouldEqual true
      }
    }

    "succeeds if a player doesn't need to act because everyone else has folded" in {
      ensurePlayersHaveFinishedActing(
        game.copy(
          players = List(
            p1.copy(
              bet = 50,
              folded = true,
            ),
            p2.copy(
              bet = 50,
              folded = true,
            ),
            p3.copy(
              bet = 50,
              stack = 0,
              checked = false,
            ),
          )
        )
      ).isRight shouldEqual true
    }

    "succeeds if a player doesn't need to act because everyone else is all-in" in {
      ensurePlayersHaveFinishedActing(
        game.copy(
          players = List(
            p1.copy(
              bet = 50,
              stack = 0,
            ),
            p2.copy(
              bet = 50,
              stack = 0,
            ),
            p3.copy(
              bet = 50,
              stack = 1000,
              checked = false,
            ),
          )
        )
      ).isRight shouldEqual true
    }

    "fails if a player has not yet bet" in {
      ensurePlayersHaveFinishedActing(
        game.copy(
          players = List(
            p1.copy(bet = 25),
            p2.copy(bet = 25),
            p3,
          )
        )
      ).isLeft shouldEqual true
    }

    "fails if a player has not yet checked" in {
      ensurePlayersHaveFinishedActing(
        game.copy(
          players = List(
            p1.copy(
              bet = 25,
              checked = true,
            ),
            p2.copy(
              bet = 25,
              checked = true,
            ),
            p3.copy(
              bet = 25,
              checked = false,
            ),
          )
        )
      ).isLeft shouldEqual true
    }

    "fails if a player checked at a lower bet amount" in {
      ensurePlayersHaveFinishedActing(
        game.copy(
          players = List(
            p1.copy(
              bet = 50,
              checked = true,
            ),
            p2.copy(
              bet = 50,
              checked = true,
            ),
            p3.copy(
              bet = 25,
              checked = true,
            ),
          )
        )
      ).isLeft shouldEqual true
    }
  }
}
