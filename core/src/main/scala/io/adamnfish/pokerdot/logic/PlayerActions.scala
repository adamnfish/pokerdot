package io.adamnfish.pokerdot.logic

import io.adamnfish.pokerdot.logic.Games._
import io.adamnfish.pokerdot.logic.Play.dealHoles
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
          // if it's just a call, other players do no need to react
          if (isCall) p
          // if this action was a bet, uncheck other player so they can respond
          else p.copy(checked = false)
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
      updatedPlayer = player.copy(
        checked = true,
      )
      updatedPlayers = game.players.map {
        case p if p.playerId == player.playerId =>
          // use updated active player in game
          updatedPlayer
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
    } yield {
      game.round.phase match {
        case PreFlop =>
          val newGame = advanceFromPreFlop(game)
          (newGame, nonBustedPlayerIds, None)
        case Flop =>
          val newGame = advanceFromFlop(game)
          (newGame, nonBustedPlayerIds, None)
        case Turn =>
          val newGame = advanceFromTurn(game)
          (newGame, nonBustedPlayerIds, None)
        case River =>
          val (newGame, playerWinnings, potWinnings) = advanceFromRiver(game)
          (newGame, nonBustedPlayerIds, Some(playerWinnings, potWinnings))
        case Showdown =>
          val newGame = startNewRound(game, rng)
          val allPlayers = game.players.map(_.playerId).toSet
          (newGame, allPlayers, None)
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
  private def advanceFromRiver(game: Game): (Game, List[PlayerWinnings], List[PotWinnings]) = {
    val gameAtRoundEnd = game.copy(
      players = game.players.map(resetPlayerForNextPhase)
    )

    val playerHands = PokerHands.bestHands(gameAtRoundEnd.round, gameAtRoundEnd.players)
    val potsWinnings = PokerHands.winnings(playerHands)
    val playersWinnings = PokerHands.playerWinnings(potsWinnings, gameAtRoundEnd.button,
      playerOrder = gameAtRoundEnd.players.map(_.playerId),
      playerHands = playerHands.map(ph => ph.player.playerId -> ph.hand),
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
    // TODO: calculate the position of the button and blinds, and see that blinds are paid
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