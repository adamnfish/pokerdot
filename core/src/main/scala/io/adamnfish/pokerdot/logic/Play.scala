package io.adamnfish.pokerdot.logic

import io.adamnfish.pokerdot.models._

import scala.util.Random
import io.adamnfish.pokerdot.logic.Utils.RichList

import scala.annotation.tailrec


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
    players.zipWithIndex.map {
      case (player, _) if player.busted =>
        player.copy(hole = None)
      case (player, i) =>
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

  def playerIsActive(player: Player): Boolean = {
    !(player.stack == 0 || player.folded || player.busted)
  }

  def playerIsInvolved(player: Player): Boolean = {
    !(player.folded || player.busted)
  }

  /**
   * If the player is in this round (i.e. not busted or folded), check if they have acted at this bid level.
   */
  def playerIsYetToAct(betAmount: Int, players: List[Player])(player: Player): Boolean = {
    if (!playerIsActive(player)) {
      // players can't act if they are out the round
      // but they also cannot act if they are all-in
      false
    } else if (player.bet < betAmount) {
      // player is expected to match the current bet amount, if able
      // if the player is all-in then they cannot act any further
      player.stack > 0
    } else {
      // player's contribution is equal to or higher than the current bid amount
      // and they are still playing in the phase
      // here we need to look at other players to decide if the round is still 'active'
      players.filter(playerIsActive) match {
        case active :: Nil if active.playerId == player.playerId =>
          // this player is the only player that can still act
          // we already checked they have matched the current bet amount
          // so they don't need to act
          // TODO: consider changing this to allow players the opportunity to fold before the all-in showdown happens?
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
    val activePlayers = players.filterNot(_.folded)
    if (activePlayers.isEmpty) 0
    else activePlayers.map(_.bet).max
  }

  def currentRaiseAmount(players: List[Player]): Int = {
    players
      .map(_.bet)
      .filter(_ > 0)
      .sorted.reverse
      .take(2) match {
      case largest :: next :: _ =>
        largest - next
      case _ =>
        0
    }
  }

  def nextPlayer(players: List[Player], currentActive: Option[PlayerId], button: Int): Option[PlayerId] = {
    val activePlayers = players.filter(playerIsActive)
    val involvedPlayers = players.filter(playerIsInvolved)

    if (involvedPlayers.length <= 1) {
      None
    } else {
      // if a player is currently active, advance to next
      val nextPlayer = for {
        activePlayerId <- currentActive
        activePlayerIndex <- players.findIndex(_.playerId == activePlayerId)
        nextIndex = (activePlayerIndex + 1) % players.length
        next <- nextActiveFromIndex(players, nextIndex)
      } yield next
      // if there is no active player to count from
      nextPlayer.orElse {
        val newRound = players.find(_.blind == BigBlind).exists(p => p.bet != 0 && p.pot == 0)
        if (activePlayers.length == 2) {
          if (newRound) {
            // in heads-up the dealer acts first in a new round
            players.findIndex(_.blind != BigBlind)
              .flatMap(nextActiveFromIndex(players, _))
          } else {
            // player after dealer for new phases within the round
            nextActiveFromIndex(players, (button + 1) % players.length)
          }
        } else {
          if (newRound) {
            // for larger games, player after big blind is first to act in a new round
            players.findIndex(_.blind == BigBlind)
              .flatMap { bigBlindIndex =>
                nextActiveFromIndex(players, (bigBlindIndex + 1) % players.length)
              }
          } else {
            // player after dealer for new phases within the round
            nextActiveFromIndex(players, (button + 1) % players.length)
          }
        }
      }
    }
  }

  // TODO: should return Either instead of using `.get`
  // TODO: handle single player remaining (winner) or no players (error)
  def nextDealerAndBlinds(players: List[Player], button: Int, smallBlindAmount: Int): (Int, List[Player]) = {
    val alivePlayers = players.filterNot(_.busted)

    if (alivePlayers.length <= 1) {
      (button, players.map(_.copy(blind = NoBlind)))
    } else {
      // small blind
      // ... is previous big blind, if still alive
      val newSmallBlindIdOpt = alivePlayers
        .find(_.blind == BigBlind)
        .map(_.playerId)

      // big blind
      // ... is next player after previous big blind
      // there is always a big blind so this should not be optional
      val newBigBlindId = players
        .findIndex(_.blind == BigBlind)
        .flatMap(i => nextAliveAfterIndex(players, i))
        .get

      // dealer
      // ... is where small blind was.
      // if they are busted or no small blind, dealer stays (or moves back to the first non-busted players)
      val newButton = {
        if (alivePlayers.length == 2) {
          // for heads-up the dealer is always the player that isn't Big Blind
          players
            .findIndex(p => p.playerId != newBigBlindId && !p.busted)
            .getOrElse(throw new RuntimeException("Couldn't find heads-up dealer"))
        } else {
          players
            // dealer is prev small blind if they are still playing
            .findIndex(p => p.blind == SmallBlind && !p.busted)
            // otherwise look backwards through active players to find player that was most recently dealer
            .getOrElse {
              def loop(i: Int): Int = {
                players.lift(i) match {
                  case Some(thisPlayer) =>
                    if (thisPlayer.playerId == newBigBlindId) {
                      // shrug, we got back round to the big blind
                      throw new RuntimeException("shrug (got back to big blind)")
                    } else if (!thisPlayer.busted) {
                      i
                    } else {
                      loop((i + players.length - 1) % players.length)
                    }
                  case None =>
                    // shrug, we got out of bounds on the list, somehow?
                    throw new RuntimeException("shrug got out of bounds?!")
                }
              }
              loop(button)
            }
        }
      }

      (
        newButton,
        players.map { p =>
          if (p.playerId == newBigBlindId)
            p.copy(
              blind = BigBlind,
              bet = math.min(p.stack, 2 * smallBlindAmount),
              stack = math.max(0, p.stack - (2 * smallBlindAmount)),
            )
          else if (newSmallBlindIdOpt.contains(p.playerId))
            p.copy(
              blind = SmallBlind,
              bet = math.min(p.stack, smallBlindAmount),
              stack = math.max(0, p.stack - smallBlindAmount),
            )
          else p.copy(
            blind = NoBlind
          )
        }
      )
    }
  }

  private[logic] def nextActiveFromIndex(players: List[Player], index: Int): Option[PlayerId] = {
    val reorderedPlayers = (players ++ players).drop(index % players.length).take(players.length)
    reorderedPlayers.find(playerIsYetToAct(currentBetAmount(players), players)).map(_.playerId)
  }

  private[logic] def nextAliveAfterIndex(players: List[Player], index: Int): Option[PlayerId] = {
    if (players.isEmpty) None
    else {
      val reorderedPlayers = (players ++ players).drop((index % players.length) + 1).take(players.length + 1)
      reorderedPlayers.find(p => !p.busted).map(_.playerId)
    }
  }
}
