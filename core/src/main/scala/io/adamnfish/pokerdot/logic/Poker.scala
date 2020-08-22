package io.adamnfish.pokerdot.logic

import io.adamnfish.pokerdot.models.{Hand, Player, Round}


object Poker {
  /**
   * Calculate the winnings for each player.
   *
   * Typically this is a single winner taking everything, but it is more
   * complex in some situations:
   *   - split pots
   *   - rounds where one or more players are all-in
   */
  def winnings(round: Round, players: List[Player]): List[(Player, Hand, Int)] = {
    // order hands by strength - ensuring ties are considered at the same time
    // from strongest to weakest, distribute winning shares until pot is empty

    val handRanking: List[List[Player]] = ???

    ???
  }
}
