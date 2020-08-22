package io.adamnfish.pokerdot.utils

import java.util.UUID

import io.adamnfish.pokerdot.logic.Cards
import io.adamnfish.pokerdot.models.Card

import scala.util.Random


object Rng {
  // Alias the State implementation we'll be using everywhere
  type Seed[A] = State[Long, A]

  def shuffledDeck[A](): Seed[List[Card]] = State { seed =>
    val random = new Random(seed)
    val shuffledDeck = random.shuffle(Cards.deck)
    val nextS = random.nextLong()
    (nextS, shuffledDeck)
  }

  def next: Seed[Long] = State { seed =>
    val newSeed = new Random(seed).nextLong()
    (newSeed, newSeed)
  }
}

/**
 * Saves having to thread RNG seeds through functions by hole.
 */
case class State[S, A](run: S => (S, A)) {
  def map[B](f: A => B): State[S, B] = State { s =>
    val (s2, a) = run(s)
    (s2, f(a))
  }

  def flatMap[B](f: A => State[S, B]): State[S, B] = State { s =>
    val (s2, a) = run(s)
    f(a).run(s2)
  }

  def value(initialState: S): A = run(initialState)._2
}
object State {
  def get[S, A]: State[S, S] = State { s =>
    (s, s)
  }

  def set[S, A](a: A): State[S, A] = State { s =>
    (s, a)
  }
}
