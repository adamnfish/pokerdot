package io.adamnfish.pokerdot

import cats.effect.IO
import io.adamnfish.pokerdot.models.Failures
import io.circe.{Json, parser}
import org.scalacheck.Gen
import org.scalactic.source.Position
import org.scalatest.compatible.Assertion
import org.scalatest.exceptions.TestFailedException
import org.scalatest.matchers.HavePropertyMatcher
import org.scalatest.matchers.should.Matchers

trait TestHelpers extends Matchers {
  val TODO: IO[Assertion] = IO.pure(assert(true))

  /** Date millis in a semi-sensible range (we don't need to worry about Long
    * overflow, for example)
    */
  val dateGen: Gen[Long] =
    Gen.chooseNum(0L, 4102444800000L)

  def having[A](
      propertyName: String,
      propertyValue: A
  ): HavePropertyMatcher[AnyRef, Any] = {
    Symbol(propertyName)(propertyValue)
  }

  implicit class HavingTestHelperString(propertyName: String) {
    infix def as[A](
        propertyValue: A
    )(implicit pos: Position): HavePropertyMatcher[AnyRef, Any] = {
      Symbol(propertyName)(propertyValue)
    }
  }

  implicit class RichEither[L, R](e: Either[L, R]) {
    def succeeded(implicit pos: Position) =
      e.fold(
        { l =>
          throw new TestFailedException(
            _ =>
              Some(
                s"The Either on which succeeded was invoked was not a Right, got Left($l)"
              ),
            None,
            pos
          )
        },
        _ => ()
      )

    def failed(implicit pos: Position) =
      e.fold(
        _ => (),
        { r =>
          throw new TestFailedException(
            _ =>
              Some(
                s"The Either on which failed was invoked was not a Left, got Right($r)"
              ),
            None,
            pos
          )
        }
      )

    def value(implicit pos: Position): R = {
      e.fold(
        { l =>
          throw new TestFailedException(
            _ =>
              Some(
                s"The Either on which value was invoked was not a Right, got Left($l)"
              ),
            None,
            pos
          )
        },
        identity
      )
    }

    def leftValue(implicit position: Position): L = {
      e.fold(
        identity,
        { r =>
          throw new TestFailedException(
            _ =>
              Some(
                s"The Either on which leftValue was invoked was not a Left, got Right($r)"
              ),
            None,
            position
          )
        }
      )
    }

    def failures()(implicit pos: Position): Failures = {
      e.fold(
        {
          case f: Failures => f
          case l =>
            throw new TestFailedException(
              _ => Some(s"Expected Failures in Left, got $l"),
              None,
              pos
            )
        },
        { r =>
          throw new TestFailedException(
            _ =>
              Some(
                s"The Either on which leftValue was invoked was not a Left, got Right($r)"
              ),
            None,
            pos
          )
        }
      )
    }
  }
}
object TestHelpers {
  def parseReq(jsonStr: String)(implicit pos: Position): Json = {
    parser.parse(jsonStr) match {
      case Left(parsingFailure) =>
        throw new TestFailedException(
          _ => Some(s"Failed to parse request JSON"),
          Some(parsingFailure),
          pos
        )
      case Right(json) =>
        json
    }
  }
}
