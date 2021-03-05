package io.adamnfish.pokerdot.logic

import io.adamnfish.pokerdot.logic.PlayerActions.{advancePhase, ensurePlayersHaveFinishedActing}
import io.adamnfish.pokerdot.{TestDates, TestHelpers, TestRng}
import io.adamnfish.pokerdot.logic.Games.{newGame, newPlayer}
import io.adamnfish.pokerdot.models.{Flop, PlayerAddress, PreFlop, River, Turn}
import org.scalacheck.Gen
import org.scalatest.OptionValues
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks


class PlayerActionsTest extends AnyFreeSpec with Matchers with TestHelpers with ScalaCheckDrivenPropertyChecks with OptionValues {
  "advancePhase" - {
    "for the simple phases" - {
      val game = newGame("Game name", trackStacks = true, TestDates, 1L)
      val p1 = newPlayer(game.gameId, "p1", isHost = false, PlayerAddress("p1-address"), TestDates)
        .copy(stack = 1000)
      val p2 = newPlayer(game.gameId, "p2", isHost = false, PlayerAddress("p2-address"), TestDates)
        .copy(stack = 1000)
      val p3 = newPlayer(game.gameId, "p3", isHost = false, PlayerAddress("p3-address"), TestDates)
        .copy(stack = 1000)
      val p4 = newPlayer(game.gameId, "p4", isHost = false, PlayerAddress("p4-address"), TestDates)
        .copy(stack = 1000)

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
            ), TestRng
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
          val (newGame, _, _) = advancePhase(testGame, TestRng).value
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
          val (newGame, _, _) = advancePhase(testGame, TestRng).value
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
          val (newGame, _, _) = advancePhase(testGame, TestRng).value
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
          val (newGame, _, _) = advancePhase(testGame, TestRng).value
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
          val (newGame, _, _) = advancePhase(testGame, TestRng).value
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
          advancePhase(testGame, TestRng).isLeft shouldEqual true
        }
      }

      "resets active player" - {
        "active player is left of the dealer" in {
          forAll(Gen.oneOf(PreFlop, Flop, Turn)) { phase =>
            val testGame = game.copy(
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
            val (newGame, _, _) = advancePhase(testGame, TestRng).value
            newGame.inTurn shouldEqual Some(p2.playerId)
          }
        }

        "active player skips ineligible players" in {
          forAll(Gen.oneOf(PreFlop, Flop, Turn)) { phase =>
            val testGame = game.copy(
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
            val (newGame, _, _) = advancePhase(testGame, TestRng).value
            newGame.inTurn shouldEqual Some(p3.playerId)
          }
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

  "ensurePlayersHaveFinishedActing" - {
    val game = newGame("Game name", trackStacks = true, TestDates, 1L)
    val p1 = newPlayer(game.gameId, "player 1", isHost = false, PlayerAddress("p1-address"), TestDates).copy(
      stack = 1000
    )
    val p3 = newPlayer(game.gameId, "player 2", isHost = false, PlayerAddress("p2-address"), TestDates).copy(
      stack = 1000
    )
    val p2 = newPlayer(game.gameId, "player 3", isHost = false, PlayerAddress("p3-address"), TestDates).copy(
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
