package io.adamnfish.pokerdot.logic

import io.adamnfish.pokerdot.models._

import scala.util.Random


/**
 * Poker functionality.
 */
object Play {
  /**
   * TODO: take player count to skip player cards (means we deal in the correct order)
   */
  def generateRound(phase: Phase, smallBlind: Int, state: Long): Round = {
    deckOrder(state) match {
      case burn1 :: flop1 :: flop2 :: flop3 :: burn2 :: turn :: burn3 :: river :: _ =>
        Round(
          phase, smallBlind,
          burn1 = burn1,
          flop1 = flop1,
          flop2 = flop2,
          flop3 = flop3,
          burn2 = burn2,
          turn = turn,
          burn3 = burn3,
          river = river,
        )
      case deck =>
        // unreachable code, asking for 8 cards from a full deck will succeed
        throw new RuntimeException(s"Unreachable code: failed to draw cards from shuffled deck `$deck`")
    }
  }

  def deckOrder(state: Long): List[Card] = {
    val random = new Random(state)
    random.shuffle(Cards.deck)
  }

  /**
   * Deals player cards, skipping the cards that will have already been used for the round.
   *
   * // TODO: take button index to deal in correct order,
   *          also deal from the top of the deck instead of after the community cards
   */
  def dealHoles(players: List[Player], deck: List[Card]): List[Player] = {
    players.filterNot(_.busted).zipWithIndex.map { case (player, i) =>
      player.copy(
        hole =
          for {
            c1 <- deck.lift((2 * i) + 8)
            c2 <- deck.lift((2 * i) + 9)
          } yield Hole(c1, c2)
      )
    }
  }

  def lookupHoles(players: List[Player]): List[(PlayerId, Hole)] = {
    for {
      activePlayer <- players
        .filterNot(_.busted)
        .filterNot(_.folded)
      privateCards <- activePlayer.hole
    } yield (activePlayer.playerId, privateCards)
  }

  /**
   * If the player is in this round (i.e. not busted or folded), check if they have acted at this bid level.
   */
  def playerIsYetToAct(betAmount: Int, players: List[Player])(player: Player): Boolean = {
    if (player.stack == 0 || player.folded || player.busted) {
      // players can't act if they are out the round
      // but they also cannot act if they are all-in
      false
    } else if (player.bet < betAmount) {
      // player is expected to match the current bet amount, if able
      // if the player is all-in then they cannot act any further
      player.stack > 0
    } else {
      // player's contribution is equal or higher than the current bid amount
      // and they are still playing in the phase
      // here we need to look at other players to decide if the round is still 'active'
      players.filterNot { p =>
        // exclude players that are out of money (all-in), or not in the round
        p.stack == 0 || p.folded || p.busted
      } match {
        case active :: Nil if active.playerId == player.playerId =>
          // this player is the only player that can still act
          // we already checked they have matched the current bet amount
          // so they don't need to act
          false
        case Nil =>
          // no active players, likely means everyone is equally all-in
          false
        case activePlayers =>
          // there are multiple active players, so it's up to this player whether they want to act
          !player.checked
      }
    }
  }

  def currentBetAmount(players: List[Player]): Int = {
    if (players.isEmpty) 0
    else players.map(_.bet).max
  }

  def nextPlayer(players: List[Player], currentActive: Option[PlayerId], button: Int): Option[PlayerId] = {
    if (players.isEmpty) None
    else {
      val nextPlayer = for {
        activePlayerId <- currentActive
        activePlayerIndex <- indexWhere(players)(_.playerId == activePlayerId)
        nextIndex = (activePlayerIndex + 1) % players.length
        next <- nextActiveFromIndex(players, nextIndex)
      } yield next

      nextPlayer.orElse {
        // if there is no active player to count from, we count from the button instead
        nextActiveFromIndex(players, (button + 1) % players.length)
      }
    }
  }

  private[logic] def nextActiveFromIndex(players: List[Player], index: Int): Option[PlayerId] = {
    val reorderedPlayers = (players ++ players).drop(index % players.length).take(players.length)
    reorderedPlayers.find(playerIsYetToAct(currentBetAmount(players), players)).map(_.playerId)
  }

  /**
   * Converts from stdlib's `-1 = empty` to an Option
   */
  private[logic] def indexWhere[A](as: List[A])(p: A => Boolean): Option[Int] = {
    val i = as.indexWhere(p)
    if (i == -1) None
    else Some(i)
  }
}
