package io.adamnfish.pokerdot.logic

import io.adamnfish.pokerdot.models._
import io.adamnfish.pokerdot.utils.Rng
import io.adamnfish.pokerdot.utils.Rng.Seed


/**
 * Poker functionality.
 */
object Play {
  def generateRound(phase: Phase): Seed[Round] = {
    for {
      deck <- Rng.shuffledDeck()
    } yield {
      deck match {
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
        case _ =>
          // unreachable code, asking for 8 cards from a full deck will succeed
          throw new RuntimeException(s"Unreachable code: failed to draw cards from shuffled deck `$deck`")
      }
    }
  }

  def hands(players: List[Player]): List[(PlayerId, Hole)] = {
    for {
      activePlayer <- players
        .filterNot(_.busted)
        .filterNot(_.folded)
      privateCards <- activePlayer.hole
    } yield (activePlayer.playerId, privateCards)
  }
}
