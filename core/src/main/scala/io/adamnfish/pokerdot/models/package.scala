package io.adamnfish.pokerdot

import zio.IO


package object models {
  type Attempt[A] = IO[Failures, A]
}
