package io.adamnfish.pokerdot.models


case class Card(
  rank: Rank,
  suit: Suit,
)

case class Hole(
  card1: Card,
  card2: Card,
)


case class Round(
  phase: Phase,
  burn1: Card,
  flop1: Card,
  flop2: Card,
  flop3: Card,
  burn2: Card,
  turn: Card,
  burn3: Card,
  river: Card,
)

sealed trait Phase
case object PreFlop  extends Phase
case object Flop     extends Phase
case object Turn     extends Phase
case object River    extends Phase
case object Showdown extends Phase

sealed trait Rank
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

sealed trait Suit
case object Clubs    extends Suit
case object Spades   extends Suit
case object Diamonds extends Suit
case object Hearts   extends Suit

case class Result(
  player: Player,
  hand: Hand,
  winnings: Int,
)

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
  kicker: Card,
  kicker2: Card,
) extends Hand
case class Straight(
  high: Card,
  next: Card,
  next2: Card,
  next3: Card,
  low: Card,
) extends Hand
case class Flush(
  high: Card,
  next: Card,
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
  next: Card,
  next2: Card,
  next3: Card,
  low: Card,
) extends Hand
