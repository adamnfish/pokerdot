package io.adamnfish.pokerdot.utils

object IntHelpers {
  def abs(i: Int): Int = {
    math.abs(math.max(i, Int.MinValue + 1))
  }
}
