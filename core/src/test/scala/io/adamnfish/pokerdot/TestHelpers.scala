package io.adamnfish.pokerdot

import io.adamnfish.pokerdot.models.{Attempt, Failures}
import org.scalacheck.Gen
import org.scalactic.source.Position
import org.scalatest.exceptions.TestFailedException
import org.scalatest.matchers.HavePropertyMatcher
import org.scalatest.matchers.should.Matchers
import org.scalatest.{Assertion, Failed, Succeeded}
import zio.Exit


trait TestHelpers extends Matchers {
  val testRuntime = zio.Runtime.default

  def having[A](propertyName: String, propertyValue: A): HavePropertyMatcher[AnyRef, Any] = {
    Symbol(propertyName) (propertyValue)
  }

  implicit class HavingTestHelperString(propertyName: String) {
    def as[A](propertyValue: A): HavePropertyMatcher[AnyRef, Any] = {
      Symbol(propertyName) (propertyValue)
    }
  }

  implicit class RichEither[L, R](e: Either[L, R]) {
    def value(implicit pos: Position): R = {
      e.fold(
        { l =>
          throw new TestFailedException(
            _ => Some(s"The Either on which value was invoked was not a Right, got Left($l)"),
            None, pos
          )
        },
        identity
      )
    }
  }

  implicit class RichAttempt[A](aa: Attempt[A]) {
    def value()(implicit pos: Position): A = {
      testRuntime.unsafeRunSync(aa) match {
        case Exit.Success(a) =>
          a
        case Exit.Failure(cause) =>
          throw new TestFailedException(
            _ => Some(s"Expected successful attempt, got failures: ${cause.failures.map(_.logString).mkString(" || ")}"),
            None, pos
          )
      }
    }

    def failures()(implicit pos: Position): Failures = {
      testRuntime.unsafeRunSync(aa) match {
        case Exit.Success(a) =>
          throw new TestFailedException(
            _ => Some(s"Expected failed attempt, got successful result: $a"),
            None, pos
          )
        case Exit.Failure(cause) =>
          Failures(cause.failures.flatMap(_.failures))
      }
    }

    def is(attemptStatus: AttemptStatus)(implicit pos: Position): Assertion = {
      testRuntime.unsafeRunSync(aa) match {
        case Exit.Success(a) =>
          if (attemptStatus == ASuccess) Succeeded
          else Failed(s"Expected failed attempt but got success `$a`").toSucceeded
        case Exit.Failure(cause) =>
          if (attemptStatus == AFailure) Succeeded
          else Failed(s"Expected successful attempt, got failures: ${cause.failures.map(_.logString).mkString(" || ")}").toSucceeded
      }
    }
  }

  sealed trait AttemptStatus
  case object ASuccess extends AttemptStatus
  case object AFailure extends AttemptStatus
}
