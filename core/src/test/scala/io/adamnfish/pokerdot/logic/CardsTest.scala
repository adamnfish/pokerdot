package io.adamnfish.pokerdot.logic

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers


class CardsTest extends AnyFreeSpec with Matchers {
  "deck" - {
    "returns a deck of 52 unique cards" in {
      Cards.deck.distinct.length shouldEqual 52
    }
  }
}
