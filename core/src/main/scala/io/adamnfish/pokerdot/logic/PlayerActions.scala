package io.adamnfish.pokerdot.logic

import io.adamnfish.pokerdot.logic.Games._
import io.adamnfish.pokerdot.logic.Play.dealHoles
import io.adamnfish.pokerdot.models._
import io.adamnfish.pokerdot.services.Rng


/**
 * This logic is quite complex so it gets its own object and tests.
 */
object PlayerActions {
  def bet(game: Game, bet: Int, player: Player): Either[Failures, (Game, Player)] = {
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
      currentBetAmount = Play.currentBetAmount(game.players)
      _ <-
        if (bet > player.stack && bet < currentBetAmount) Left {
          if (currentBetAmount > player.stack) {
            Failures(
              "Player needs to go all-in to bet",
              "You will have to go all-in to keep playing in this round.",
            )
          } else {
            Failures(
              "Bet must match other players' bets",
              "Your bet must be at least as much as the other players have paid.",
            )
          }
        } else Right(())
      // uncheck other players, they will need the opportunity to respond to this bet
      checkedPlayers = game.players.map(_.copy(checked = false))
      // update this player's moneys, and deactivate
      updatedPlayer = player.copy(
        bet = player.bet + bet,
        stack = player.stack - bet,
      )
      updatedPlayers = checkedPlayers.map {
        case p if p.playerId == player.playerId =>
          // use updated active player in game
          updatedPlayer
        case p => p
      }
      nextActivePlayer = Play.nextPlayer(updatedPlayers, Some(player.playerId), game.button)
    } yield (
      game.copy(
        players = updatedPlayers,
        inTurn = nextActivePlayer,
      ),
      updatedPlayer
    )
  }

  def check(game: Game, player: Player): Either[Failures, (Game, Player)] = {
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
    } yield (
      // update active player in game and update active player
      game.copy(
        players = updatedPlayers,
        inTurn = nextActivePlayer,
      ),
      updatedPlayer
    )
  }

  def fold(game: Game, player: Player): (Game, Player) = {
    val updatedPlayer = player.copy(
      folded = true,
    )
    val updatedPlayers = game.players.map {
      case p if p.playerId == player.playerId =>
        // use updated active player in game
        updatedPlayer
      case p => p
    }
    // TODO: be careful here if we allow off-turn folds
    val nextActivePlayer = Play.nextPlayer(updatedPlayers, Some(player.playerId), game.button)
    (
      game.copy(
        players = updatedPlayers,
        inTurn = nextActivePlayer,
      ),
      updatedPlayer
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
          s"Cannot advance phase while ${playersYetToAct.length} players have not yet acted",
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
    val playerHands = PokerHands.bestHands(game.round, game.players)
    val potsWinnings = PokerHands.winnings(playerHands)
    val playersWinnings = PokerHands.playerWinnings(potsWinnings, game.button,
      playerOrder = game.players.map(_.playerId),
      playerHands = playerHands.map(ph => ph.player.playerId -> ph.hand),
    )
    val updatedPlayers = game.players.map(resetPlayerForShowdown(playersWinnings))
    (
      game.copy(
        round = game.round.copy(phase = Showdown),
        inTurn = None,
        players = updatedPlayers,
      ),
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
    val newButton = game.button % updatedPlayers.length
    // TODO: calculate the position of the button and blinds, and see that blinds are paid
    game.copy(
      round = game.round.copy(phase = PreFlop),
      button = newButton, // dealer advances
      inTurn = Play.nextPlayer(updatedPlayers, None, newButton),
      players = dealHoles(game.players.map(resetPlayerForNextRound), nextDeck),
      seed = nextState
    )
  }
}
