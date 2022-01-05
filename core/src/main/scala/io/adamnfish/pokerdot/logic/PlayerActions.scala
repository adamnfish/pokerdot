package io.adamnfish.pokerdot.logic

import io.adamnfish.pokerdot.logic.Games._
import io.adamnfish.pokerdot.logic.Play.{dealHoles, playerIsActive, playerIsInvolved}
import io.adamnfish.pokerdot.models._
import io.adamnfish.pokerdot.services.{Clock, Rng}


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
              "you will have to go all-in to keep playing in this round.",
            )
          } else {
            Failures(
              s"Bet ($bet) must match other players' bets ($currentBetAmount)",
              "your bet must be at least as much as the other players have paid.",
            )
          }
        } else Right(())
      // ensure raise amount matches previous raise
      _ <-
        if (!allIn && isRaise && (betTotal - currentBetAmount) < Play.currentRaiseAmount(game.players)) Left {
          Failures(
            "Raise amount does not meet previous raises",
            "you must raise by at least as much as the last bet or raise.",
          )
        } else Right(())
      // ensure raise amount matches minimum raise (big blind)
      _ <-
        if (!allIn && isRaise && (betTotal - currentBetAmount) < game.round.smallBlind * 2) Left {
          Failures(
            "Player needs to raise by at least the Big Blind",
            "the minimum raise is the Big Blind.",
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
            "you have to at least call other players before checking.",
          )
        } else Right(())
      _ <-
        if (player.checked) Left {
          Failures(
            "Player is already checked",
            "you have already checked.",
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
  def advancePhase(game: Game, clock: Clock, rng: Rng): Either[Failures, (Game, Set[PlayerId], Option[(List[PlayerWinnings], List[PotWinnings])])] = {
    for {
      _ <- ensurePlayersHaveFinishedActing(game)
      nonBustedPlayerIds = game.players.filterNot(_.busted).map(_.playerId).toSet
      involvedPlayers = game.players.filter(playerIsInvolved)
      foldedFinish = involvedPlayers.length == 1
      result <- (foldedFinish, game.round.phase) match {
        // only progress through standard phases while players are still playing
        case (false, PreFlop) =>
          val newGame = advanceFromPreFlop(game)
          Right((newGame, nonBustedPlayerIds, None))
        case (false, Flop) =>
          val newGame = advanceFromFlop(game)
          Right((newGame, nonBustedPlayerIds, None))
        case (false, Turn) =>
          val newGame = advanceFromTurn(game)
          Right((newGame, nonBustedPlayerIds, None))
        case (false, River) =>
          val (newGame, playerWinnings, potWinnings) = advanceFromRiver(game)
          Right((newGame, nonBustedPlayerIds, Some(playerWinnings, potWinnings)))
        case (_, Showdown) =>
          // we can proceed from showdown whenever 2 or more players are still in the game
          startNewRound(game, clock, rng).map { newGame =>
            val allPlayerIds = game.players.map(_.playerId).toSet
            (newGame, allPlayerIds, None)
          }
        case (true, _) =>
          // skip straight to showdown from any phase if everyone else has folded
          val (newGame, playerWinning, potWinning) = advanceFromFoldedFinish(game)
          Right((newGame, nonBustedPlayerIds, Some(List(playerWinning), List(potWinning))))
      }
    } yield result
  }

  def updateBlind(game: Game, updateBlind: UpdateBlind, now: Long): Either[Failures, Game] = {
    for {
      newGame <- (updateBlind.smallBlind, updateBlind.timerLevels, updateBlind.playing, updateBlind.progress, game.timer) match {
        case (Some(_), _, Some(playing), _, _) =>
          val status = if (playing) "start" else "pause"
          Left(Failures(s"Cannot $status a timer when using manual blinds", s"you can't $status a timer if you're using manual blinds"))
        case (Some(_), Some(_), _, _, _) =>
          Left(Failures("Cannot set timer levels when using manual blinds", "you can't create a timer if you're using manual blinds"))
        case (Some(_), _, _, Some(_), _) =>
          Left(Failures("Cannot set timer progress when using manual blinds", "you can't update a timer if you're using manual blinds"))
        case (None, None, Some(playing), None, None) =>
          val status = if (playing) "start" else "pause"
          Left(Failures(s"Cannot $status timer that does not exist", s"there's no timer running so we can't $status it"))
        case (None, None, _, Some(_), None) =>
          Left(Failures(s"Cannot update progress on a timer that does not exist", s"there's no timer running so we can't update it"))
        case (Some(manualSmallBlind), None, None, None, _) =>
          // use manual blinds
          Right {
            game.copy(
              round = game.round.copy(smallBlind = manualSmallBlind),
              timer = None,
            )
          }
        case (None, Some(timerLevels), maybeTimerRunning, maybeProgress, None) =>
          // set new timer
          val pausedTime =
            if (maybeTimerRunning.getOrElse(true)) None
            else Some(now)
          val startTime =
            now - maybeProgress.map(_ * 1000).getOrElse(0)
          val initialBlind =
            timerLevels
              .collectFirst { case RoundLevel(_, smallBlind) => smallBlind }
              .getOrElse(0) // this should be excluded by validation
          Right {
            game.copy(
              round = game.round.copy(smallBlind = initialBlind),
              timer = Some(TimerStatus(startTime, pausedTime, timerLevels)),
            )
          }
        case (None, None, None, Some(newProgress), Some(existingTimer)) =>
          // set the progress of an existing timer
          val newTimer = existingTimer.pausedTime match {
            case Some(pausedTime) =>
              existingTimer.copy(timerStartTime = pausedTime - (newProgress * 1000))
            case None =>
              existingTimer.copy(timerStartTime = now - (newProgress * 1000))
          }
          Right {
            game.copy(
              timer = Some(newTimer),
            )
          }
        case (None, maybeTimerLevels, maybeTimerRunning, maybeProgress, Some(existingTimer)) =>
          // update an existing timer
          for {
            newTimerStatus <- updateBlindTimer(existingTimer, now, maybeTimerRunning, maybeProgress, maybeTimerLevels)
            result <- Play.timerSmallBlind(newTimerStatus, now)
            (newSmallBlind, _) = result
          } yield game.copy(
            round =
              // playing / pausing shouldn't cause the current round's blind to change,
              // even if the timer has moved on to the next level (this takes effect when the new round starts)
              // if the blinds have been explicitly edited then we'll update the round in-place
              if (maybeTimerLevels.isDefined)
                game.round.copy(
                  smallBlind = newSmallBlind,
                )
              else game.round,
            timer = Some(newTimerStatus),
          )
        case (None, None, None, None, None) =>
          // ?? should be excluded by validation of the update blind request
          Right(game)
      }
    } yield newGame
  }

  private def updateBlindTimer(currentTimerStatus: TimerStatus, now: Long, maybeSetPlayingStatus: Option[Boolean], maybeProgress: Option[Int], newLevels: Option[List[TimerLevel]]): Either[Failures, TimerStatus] = {
    (currentTimerStatus.pausedTime, maybeSetPlayingStatus) match {
      case (Some(_), Some(false)) =>
        // already paused
        Left(Failures("Cannot pause timer when it's already paused", "the timer is already paused."))
      case (None, Some(true)) =>
        // already playing
        Left(Failures("Cannot start timer when it's already running", "the timer is already running."))
      case (Some(currentPausedTime), Some(true)) =>
        // unpause, which adjusts the start time to put the timer in the right place
        Right {
          TimerStatus(
            timerStartTime = maybeProgress match {
              case Some(progress) =>
                now - (progress * 1000)
              case None =>
                currentTimerStatus.timerStartTime + (now - currentPausedTime)
            },
            pausedTime = None,
            levels = newLevels.getOrElse(currentTimerStatus.levels)
          )
        }
      case (None, Some(false)) =>
        // pause
        Right {
          TimerStatus(
            timerStartTime = maybeProgress match {
              case Some(progress) =>
                now - (progress * 1000)
              case None =>
                currentTimerStatus.timerStartTime
            },
            pausedTime = Some(now),
            levels = newLevels.getOrElse(currentTimerStatus.levels)
          )
        }
      case (_, None) =>
        // not setting the play/pause status, so we can just go ahead and update the progress and levels
        Right {
          TimerStatus(
            timerStartTime = maybeProgress match {
              case Some(progress) =>
                now - (progress * 1000)
              case None =>
                currentTimerStatus.timerStartTime
            },
            pausedTime = currentTimerStatus.pausedTime,
            levels = newLevels.getOrElse(currentTimerStatus.levels)
          )
        }
      case _ =>
        // to get here implies a contradiction between the current and desired play/pause states
        // this should be excluded earlier in the request lifecycle, so nothing to do here
        Left {
          Failures(
            s"Unexpected application state. is playing: ${currentTimerStatus.pausedTime.isEmpty}, desired playing state ${maybeSetPlayingStatus}",
            "couldn't understand the timer update, maybe try refreshing?"
          )
        }
    }
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
        .flatMap { ph =>
          ph.player.hole.map { hole =>
            (ph.player.playerId, ph.hand, hole)
          }
        },
    )
    val updatedPlayers = gameAtRoundEnd.players.map(resetPlayerForShowdown(playersWinnings))
    (
      gameAtRoundEnd.copy(
        round = gameAtRoundEnd.round.copy(phase = Showdown),
        inTurn = None,
        players = updatedPlayers,
      ),
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
        // TODO: return attempt rather than get the hole
        //       (we know there will be a hole here though so this is safe)
        val playerWinnings = PlayerWinnings(winnerId, None, singleActivePlayer.hole.get, potSize)
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
   *
   * If fewer than 2 players remain, the game is finished and we should not proceed.
   */
  private[logic] def startNewRound(game: Game, clock: Clock, rng: Rng): Either[Failures, Game] = {
    // finalise player payments, reset (and bust) players
    // shuffle, deal new cards, set up new round
    val nextState = rng.nextState(game.seed)
    val nextDeck = Play.deckOrder(nextState)
    val updatedPlayers = game.players.map(resetPlayerForNextRound)
    if (updatedPlayers.count(!_.busted) < 2) {
      Left(Failures(
        "Cannot advance from finished game showdown",
        "you can't start a new round because the game has finished",
      ))
    } else {
      // TODO: check whether blind amounts should change based on timer
      Play.blindForNextRound(game.round.smallBlind, clock.now(), game.timer).map { newSmallBlind =>
        val (newButton, blindUpdatedPlayers) = Play.nextDealerAndBlinds(updatedPlayers, game.button, newSmallBlind)
        game.copy(
          round = game.round.copy(
            phase = PreFlop,
            smallBlind = newSmallBlind,
          ),
          button = newButton, // dealer advances
          inTurn = Play.nextPlayer(blindUpdatedPlayers, None, newButton),
          players = dealHoles(blindUpdatedPlayers, nextDeck),
          seed = nextState
        )
      }
    }
  }
}
