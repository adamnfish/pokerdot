package io.adamnfish.pokerdot.logic

import io.adamnfish.pokerdot.models.{Ace, Card, Clubs, Diamonds, Eight, Five, Four, Hearts, Jack, King, Nine, Queen, Rank, Seven, Six, Spades, Suit, Ten, Three, Two}


object Cards {
  implicit class RichRank(rank: Rank) {
    def of(suit: Suit): Card = {
      Card(rank, suit)
    }
  }

  val deck =
    for {
      suit <- List(
        Clubs, Spades, Diamonds, Hearts,
      )
      rank <- List(
        Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace,
      )
    } yield rank of suit

  def rankStr(rank: Rank): String = {
    rank match {
      case Two =>
        "2"
      case Three =>
        "3"
      case Four =>
        "4"
      case Five =>
        "5"
      case Six =>
        "6"
      case Seven =>
        "7"
      case Eight =>
        "8"
      case Nine =>
        "9"
      case Ten =>
        "10"
      case Jack =>
        "J"
      case Queen =>
        "Q"
      case King =>
        "K"
      case Ace =>
        "A"
    }
  }

  def suitStr(suit: Suit): String = {
    suit match {
      case Clubs =>
        "♣"
      case Spades =>
        "♠"
      case Diamonds =>
        "♦"
      case Hearts =>
        "♥"
    }
  }

  def cardStr(card: Card): String = {
    s"${rankStr(card.rank)}${suitStr(card.suit)}"
  }
}
