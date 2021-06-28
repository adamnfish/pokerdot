package io.adamnfish.pokerdot.logic

import io.adamnfish.pokerdot.logic.Games._
import io.adamnfish.pokerdot.logic.Play.{dealHoles, playerIsActive, playerIsInvolved}
import io.adamnfish.pokerdot.models._
import io.adamnfish.pokerdot.services.Rng


/**
 * This logic is quite complex so it gets its own object and tests.
 */
object PlayerActions {
  def bet(game: Game, bet: Int, player: Player): Either[Failures, (Game, ActionSummary)] = {
    val allIn = bet == player.stack
    val betTotal = player.bet + bet
    val currentBetAmount = Play.currentBetAmount(game.players)
    val isCall = betTotal == currentBetAmount
    val isRaise = betTotal > currentBetAmount
    for {
      // ensure bet amount does not exceed stack
      _ <-
        if (bet > player.stack) Left {
          Failures(
            "Bet cannot exceed player stack",
            "You can't afford that bet.",
          )
        } else Right(())
      // ensure bet matches other players' contributions this round
      _ <-
        if (!allIn && betTotal < currentBetAmount) Left {
          if (currentBetAmount > player.stack) {
            Failures(
              "Player needs to go all-in to bet",
              "You will have to go all-in to keep playing in this round.",
            )
          } else {
            Failures(
              s"Bet ($bet) must match other players' bets ($currentBetAmount)",
              "Your bet must be at least as much as the other players have paid.",
            )
          }
        } else Right(())
      // ensure raise amount matches previous raise
      _ <-
        if (!allIn && isRaise && (betTotal - currentBetAmount) < Play.currentRaiseAmount(game.players)) Left {
          Failures(
            "Raise amount does not meet previous raises",
            "You must raise by at least as much as the last bet or raise.",
          )
        } else Right(())
      // ensure raise amount matches minimum raise (big blind)
      _ <-
        if (!allIn && isRaise && (betTotal - currentBetAmount) < game.round.smallBlind * 2) Left {
          Failures(
            "Player needs to raise by at least the Big Blind",
            "The minimum raise is the Big Blind.",
          )
        } else Right(())
      updatedPlayers = game.players.map {
        case thisPlayer if thisPlayer.playerId == player.playerId =>
          // use updated active player in game
          player.copy(
            bet = betTotal,
            stack = player.stack - bet,
            // calling a bet doesn't give you another chance to act
            // but after betting / raising you also can't react unless re-raised
            checked = true,
          )
        case p =>
          // if this action was a bet, uncheck other player so they can respond
          if (isRaise) p.copy(checked = false)
          // if it's just a call (or small all-in), other players do not need to react
          else p
      }
      nextActivePlayer = Play.nextPlayer(updatedPlayers, Some(player.playerId), game.button)
    } yield (
      game.copy(
        players = updatedPlayers,
        inTurn = nextActivePlayer,
      ),
      if (isCall) CallSummary(player.playerId)
      else BetSummary(player.playerId, bet)
    )
  }

  def check(game: Game, player: Player): Either[Failures, Game] = {
    val currentBetAmount = Play.currentBetAmount(game.players)
    for {
      // ensure player is allowed to check
      _ <-
        if (player.bet < currentBetAmount) Left {
          Failures(
            "Player cannot check until they have called other players",
            "You have to at least call other players before checking.",
          )
        } else Right(())
      _ <-
        if (player.checked) Left {
          Failures(
            "Player is already checked",
            "You have already checked.",
          )
        } else Right(())
      updatedPlayers = game.players.map {
        case p if p.playerId == player.playerId =>
          // use updated active player in game
          player.copy(
            checked = true,
          )
        case p => p
      }
      // calculate next active player
      // TODO: be careful here if we allow off-turn checks
      nextActivePlayer = Play.nextPlayer(updatedPlayers, Some(player.playerId), game.button)
    } yield {
      // update active player in game and update active player
      game.copy(
        players = updatedPlayers,
        inTurn = nextActivePlayer,
      )
    }
  }

  def fold(game: Game, player: Player): Game = {
    val updatedPlayers = game.players.map {
      case p if p.playerId == player.playerId =>
        // use updated active player in game
        player.copy(
          folded = true,
        )
      case p => p
    }
    // TODO: be careful here if we allow off-turn folds
    val nextActivePlayer = Play.nextPlayer(updatedPlayers, Some(player.playerId), game.button)
    game.copy(
      players = updatedPlayers,
      inTurn = nextActivePlayer,
    )
  }

  /**
   * Checks the round is ready to be advanced, then delegates
   * to the current round's advancement logic.
   */
  def advancePhase(game: Game, rng: Rng): Either[Failures, (Game, Set[PlayerId], Option[(List[PlayerWinnings], List[PotWinnings])])] = {
    for {
      _ <- ensurePlayersHaveFinishedActing(game)
      nonBustedPlayerIds = game.players.filterNot(_.busted).map(_.playerId).toSet
      involvedPlayers = game.players.filter(playerIsInvolved)
    } yield {
      val foldedFinish = involvedPlayers.length == 1
      (foldedFinish, game.round.phase) match {
        // only progress through standard phases while players are still playing
        case (false, PreFlop) =>
          val newGame = advanceFromPreFlop(game)
          (newGame, nonBustedPlayerIds, None)
        case (false, Flop) =>
          val newGame = advanceFromFlop(game)
          (newGame, nonBustedPlayerIds, None)
        case (false, Turn) =>
          val newGame = advanceFromTurn(game)
          (newGame, nonBustedPlayerIds, None)
        case (false, River) =>
          val (newGame, playerWinnings, potWinnings) = advanceFromRiver(game)
          (newGame, nonBustedPlayerIds, Some(playerWinnings, potWinnings))
        case (_, Showdown) =>
          // we can proceed from showdown however many players are left
          val newGame = startNewRound(game, rng)
          val allPlayerIds = game.players.map(_.playerId).toSet
          (newGame, allPlayerIds, None)
        case (true, _) =>
          // skip straight to showdown if everyone else has folded
          val (newGame, playerWinning, potWinning) = advanceFromFoldedFinish(game)
          (newGame, nonBustedPlayerIds, Some(List(playerWinning), List(potWinning)))
      }
    }
  }

  def updateBlind(game: Game, updateBlind: UpdateBlind, now: Long): Either[Failures, Game] = {
    val newSmallBlind = updateBlind.smallBlind.getOrElse(game.round.smallBlind)
    val currentlyPlaying = !game.timer.exists(_.pausedTime.isDefined)

    for {
      newTimerStatus <-
        (updateBlind.smallBlind, updateBlind.playing, updateBlind.timerLevels) match {
          // TODO: can be much simpler!
          case (Some(_), _, _) =>
            // if we're manually setting the blind then the timer will be removed
            Right(None)
          case (_, Some(playing), Some(timerLevels)) =>
            if (playing == currentlyPlaying && playing) {
              Left(Failures("Cannot start timer when it's already running", "The timer is already running."))
            } else if (playing == currentlyPlaying && !playing) {
              Left(Failures("Cannot pause timer when it's already paused", "The timer is already paused."))
            } else {
              Right {
                game.timer.map { timerStatus =>
                  val pausedTime =
                    if (playing) None
                    else Some(now)
                  timerStatus.copy(
                    pausedTime = pausedTime,
                    levels = timerLevels,
                  )
                }
              }
            }
          case (_, Some(playing), None) =>
            if (playing == currentlyPlaying && playing) {
              Left(Failures("Cannot start timer when it's already running", "The timer is already running."))
            } else if (playing == currentlyPlaying && !playing) {
              Left(Failures("Cannot pause timer when it's already paused", "The timer is already paused."))
            } else {
              Right {
                game.timer.map { timerStatus =>
                  val pausedTime =
                    if (playing) None
                    else Some(now)
                  timerStatus.copy(pausedTime = pausedTime)
                }
              }
            }
          case (_, None, Some(timerLevels)) =>
            Right(Some {
              game.timer.map { timerStatus =>
                timerStatus.copy(levels = timerLevels)
              }.getOrElse {
                TimerStatus(
                  now, None, timerLevels
                )
              }
            })
          case _ =>
            Right(game.timer)
        }
    } yield game.copy(
      round = game.round.copy(smallBlind = newSmallBlind),
      timer = newTimerStatus,
    )
  }

  private[logic] def ensurePlayersHaveFinishedActing(game: Game): Either[Failures, Unit] = {
    val betAmount = Play.currentBetAmount(game.players)
    val playersYetToAct = game.players.filter(Play.playerIsYetToAct(betAmount, game.players))
    if (playersYetToAct.nonEmpty) {
      val message =
        playersYetToAct match {
          case lastPlayer :: Nil =>
            s"${lastPlayer.screenName} needs to act before the round is finished"
          case _ =>
            s"${playersYetToAct.size} players still need to act"
        }
      Left(
        Failures(
          s"Cannot advance phase when players have not yet acted: ${playersYetToAct.map(_.playerId.pid)}",
          message,
        )
      )
    } else {
      Right(())
    }
  }

  /**
   * Updates the game after the first round of betting,
   * before any community cards have been dealt.
   */
  private def advanceFromPreFlop(game: Game): Game = {
    val updatedPlayers = game.players.map(resetPlayerForNextPhase)
    game.copy(
      round = game.round.copy(phase = Flop),
      inTurn = Play.nextPlayer(updatedPlayers, game.inTurn, game.button),
      players = updatedPlayers,
    )
  }

  /**
   * Updates the game after the second round of betting.
   * The first three community cards have been revealed.
   */
  private def advanceFromFlop(game: Game): Game = {
    val updatedPlayers = game.players.map(resetPlayerForNextPhase)
    game.copy(
      round = game.round.copy(phase = Turn),
      inTurn = Play.nextPlayer(updatedPlayers, game.inTurn, game.button),
      players = updatedPlayers,
    )
  }

  /**
   * Updates the game after the third round of betting,
   * four community cards have been revealed.
   */
  private def advanceFromTurn(game: Game): Game = {
    val updatedPlayers = game.players.map(resetPlayerForNextPhase)
    game.copy(
      round = game.round.copy(phase = River),
      inTurn = Play.nextPlayer(updatedPlayers, game.inTurn, game.button),
      players = updatedPlayers,
    )
  }

  /**
   * Update the game after the final round of betting, with all
   * community cards visible.
   *
   * Since all betting is now complete, we must calculate the
   * result of the round.
   */
  private[logic] def advanceFromRiver(game: Game): (Game, List[PlayerWinnings], List[PotWinnings]) = {
    val gameAtRoundEnd = game.copy(
      players = game.players.map(resetPlayerForNextPhase)
    )

    val playerHands = PokerHands.bestHands(gameAtRoundEnd.round, gameAtRoundEnd.players)
    val potsWinnings = PokerHands.winnings(playerHands)
    val playersWinnings = PokerHands.playerWinnings(potsWinnings, gameAtRoundEnd.button,
      playerOrder = gameAtRoundEnd.players.map(_.playerId),
      playerHands = playerHands
        .filterNot(_.player.folded)
        .map(ph => ph.player.playerId -> ph.hand),
    )
    val updatedPlayers = gameAtRoundEnd.players.map(resetPlayerForShowdown(playersWinnings))
    (
      gameAtRoundEnd.copy(
        round = gameAtRoundEnd.round.copy(phase = Showdown),
        inTurn = None,
        players = updatedPlayers,
      ),
      // TODO: exclude folded players from hand display
      playersWinnings,
      potsWinnings
    )
  }

  private[logic] def advanceFromFoldedFinish(game: Game): (Game, PlayerWinnings, PotWinnings) = {
    val gameAtRoundEnd = game.copy(
      players = game.players.map(resetPlayerForNextPhase)
    )

    gameAtRoundEnd.players.filter(playerIsInvolved) match {
      case singleActivePlayer :: Nil =>
        val potSize = gameAtRoundEnd.players.map(_.pot).sum
        val winnerId = singleActivePlayer.playerId
        val playerWinnings = PlayerWinnings(winnerId, None, potSize)
        (
          gameAtRoundEnd.copy(
            round = gameAtRoundEnd.round.copy(phase = Showdown),
            inTurn = None,
            players = gameAtRoundEnd.players.map(resetPlayerForShowdown(List(playerWinnings))),
          ),
          playerWinnings,
          PotWinnings(potSize, Set(winnerId), Set(winnerId)),
        )
      case incorrectNumberOfActivePlayers =>
        throw new RuntimeException(s"advanceFromFoldedFinish with ${incorrectNumberOfActivePlayers.length} players: Unreachable code, only call this fn when a single player remains")
    }
  }

  /**
   * Advancing from the showdown will setup and start the next round.
   * This means resetting the game and player states for a new round of Poker.
   */
  private def startNewRound(game: Game, rng: Rng): Game = {
    // finalise player payments, reset (and bust) players
    // shuffle, deal new cards, set up new round
    val nextState = rng.nextState(game.seed)
    val nextDeck = Play.deckOrder(nextState)
    val updatedPlayers = game.players.map(resetPlayerForNextRound)
    // TODO: check whether blind amounts should change based on timer
    val (newButton, blindUpdatedPlayers) = Play.nextDealerAndBlinds(updatedPlayers, game.button, game.round.smallBlind)
    game.copy(
      round = game.round.copy(phase = PreFlop),
      button = newButton, // dealer advances
      inTurn = Play.nextPlayer(blindUpdatedPlayers, None, newButton),
      players = dealHoles(blindUpdatedPlayers, nextDeck),
      seed = nextState
    )
  }
}
