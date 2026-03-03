package io.adamnfish.pokerdot.services

import cats.effect.IO
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.effect.PropF


class RngTest extends CatsEffectSuite with ScalaCheckEffectSuite {
  test("production RNG returns a random initial seed") {
    val rng = new RandomRng[IO]
    for
      nrd1 <- rng.randomState
      rnd2 <- rng.randomState
    yield assertNotEquals(nrd1, rnd2)
  }
  
  test("production RNG returns a different 'next' seed every time") {
    PropF.forAllF { (seed: Long) =>
      val rng = new RandomRng[IO]
      for
        nrd1 <- rng.nextState(seed)
        rnd2 <- rng.nextState(seed)
      yield assertNotEquals(nrd1, rnd2)
    }
  }
}
