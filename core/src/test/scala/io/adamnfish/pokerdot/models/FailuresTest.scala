package io.adamnfish.pokerdot.models

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers


class FailuresTest extends AnyFreeSpec with Matchers {
  "externalFailures" - {
    "returns all failures in the typical case" in {
      val failures = List(
        Failure("test message 1", "nice test info 1"),
        Failure("test message 2", "nice test info 2"),
        Failure("test message 3", "nice test info 3"),
      )
      Failures(failures).externalFailures shouldEqual failures
    }

    "returns some external failures if present" in {
      val externalFailure = Failure("test message 2", "nice test info 2")
      val result = Failures(List(
        Failure("test message 1", "nice test info 1", internal = true),
        externalFailure,
        Failure("test message 3", "nice test info 3", internal = true),
      )).externalFailures

      result shouldEqual List(externalFailure)
    }

    "returns an empty list if all failures are internal" in {
      val failures = List(
        Failure("test message 1", "nice test info 1", internal = true),
        Failure("test message 2", "nice test info 2", internal = true),
        Failure("test message 3", "nice test info 3", internal = true),
      )
      Failures(failures).externalFailures shouldEqual Nil
    }
  }
}
