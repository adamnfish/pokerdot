package io.adamnfish.pokerdot.models

import io.adamnfish.pokerdot.logic.Cards


case class Card(
  rank: Rank,
  suit: Suit,
) {
  override def toString: String = Cards.cardStr(this)
}

case class Hole(
  card1: Card,
  card2: Card,
)


case class Round(
  phase: Phase,
  smallBlind: Int,
  burn1: Card,
  flop1: Card,
  flop2: Card,
  flop3: Card,
  burn2: Card,
  turn: Card,
  burn3: Card,
  river: Card,
)

sealed trait Phase extends Product
case object PreFlop  extends Phase
case object Flop     extends Phase
case object Turn     extends Phase
case object River    extends Phase
case object Showdown extends Phase

sealed trait Blind extends Product
case object NoBlind    extends Blind
case object SmallBlind extends Blind
case object BigBlind   extends Blind

sealed trait Rank extends Product {
  override def toString: String = Cards.rankStr(this)
}
case object Two   extends Rank
case object Three extends Rank
case object Four  extends Rank
case object Five  extends Rank
case object Six   extends Rank
case object Seven extends Rank
case object Eight extends Rank
case object Nine  extends Rank
case object Ten   extends Rank
case object Jack  extends Rank
case object Queen extends Rank
case object King  extends Rank
case object Ace   extends Rank

sealed trait Suit extends Product {
  override def toString: String = Cards.suitStr(this)
}
case object Clubs    extends Suit
case object Diamonds extends Suit
case object Spades   extends Suit
case object Hearts   extends Suit

sealed trait Hand extends Product
case class HighCard(
  highCard: Card,
  kicker1: Card,
  kicker2: Card,
  kicker3: Card,
  kicker4: Card,
) extends Hand
case class Pair(
  pair1: Card,
  pair2: Card,
  kicker1: Card,
  kicker2: Card,
  kicker3: Card,
) extends Hand
case class TwoPair(
  up1: Card,
  up2: Card,
  down1: Card,
  down2: Card,
  kicker: Card,
) extends Hand
case class ThreeOfAKind(
  trip1: Card,
  trip2: Card,
  trip3: Card,
  kicker1: Card,
  kicker2: Card,
) extends Hand
case class Straight(
  high: Card,
  next1: Card,
  next2: Card,
  next3: Card,
  low: Card,
) extends Hand
case class Flush(
  high: Card,
  next1: Card,
  next2: Card,
  next3: Card,
  low: Card,
) extends Hand
case class FullHouse(
  trip1: Card,
  trip2: Card,
  trip3: Card,
  pair1: Card,
  pair2: Card,
) extends Hand
case class FourOfAKind(
  quad1: Card,
  quad2: Card,
  quad3: Card,
  quad4: Card,
  kicker: Card,
) extends Hand
case class StraightFlush(
  high: Card,
  next1: Card,
  next2: Card,
  next3: Card,
  low: Card,
) extends Hand

case class PlayerHand(
  player: Player,
  hand: Hand,
)

case class PlayerWinnings(
  playerId: PlayerId,
  hand: Hand,
  winnings: Int,
)

case class PotWinnings(
  potSize: Int,
  participants: Set[PlayerId],
  winners: Set[PlayerId],
)
