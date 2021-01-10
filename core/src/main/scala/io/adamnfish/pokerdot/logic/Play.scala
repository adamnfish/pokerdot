package io.adamnfish.pokerdot.logic

import io.adamnfish.pokerdot.models._

import scala.util.Random


/**
 * Poker functionality.
 */
object Play {
  def generateRound(phase: Phase, state: Long): Round = {
    deckOrder(state) match {
      case burn1 :: flop1 :: flop2 :: flop3 :: burn2 :: turn :: burn3 :: river :: _ =>
        Round(
          phase,
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
   * Deals player cards, skipping the cards that will have already been used for the round
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
  def playerIsYetToAct(betAmount: Int)(player: Player): Boolean = {
    if (player.folded || player.busted) {
      false
    } else if (player.bet < betAmount) {
      true
    } else {
      // player's contribution is equal (or higher - shouldn't happen) than the current bid amount
      // and they are still playing in the phase
      !player.checked
    }
  }
}
