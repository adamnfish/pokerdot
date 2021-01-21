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
   * The dealer does not necessarily advance round the table each round. Rather,
   * the goal is to ensure that the every player gets their turn with the Big Blind.
   *
   * This function works out the next arrangement of blinds and button and returns
   * the index of the dealer/button, the small blind (which may be omitted in a round),
   * and the big blind.
   */
  def setupNextRound(players: List[Player], currentButton: Int, smallBlind: Int): (Int, Option[Int], Int) = {
    val playerCount = players.length
    val playersInRoundOrder = (players ++ players).drop(currentButton % playerCount).take(playerCount)
    val newBigBlindIndex = -1
    ???
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

  def currentBetAmount(players: List[Player]): Int = {
    players.filterNot(p => p.busted || p.folded).map(_.bet).max
  }

  def nextPlayer(players: List[Player], currentActive: Option[PlayerId], button: Int): Option[PlayerId] = {
    val nextPlayer = for {
      activePlayerId <- currentActive
      activePlayerIndex <- {
        val i = players.indexWhere(_.playerId == activePlayerId)
        if (i == -1) None
        else Some(i)
      }
      nextIndex = (activePlayerIndex + 1) % players.length
      next <- nextActiveFromIndex(players, nextIndex)
    } yield next

    nextPlayer.orElse {
      // back to start player if there is no active player
      nextActiveFromIndex(players, (button + 1) % players.length)
    }
  }

  private[logic] def nextActiveFromIndex(players: List[Player], index: Int): Option[PlayerId] = {
    val reorderedPlayers = (players ++ players).drop(index % players.length).take(players.length)
    reorderedPlayers.find(playerIsYetToAct(currentBetAmount(players))).map(_.playerId)
  }
}
