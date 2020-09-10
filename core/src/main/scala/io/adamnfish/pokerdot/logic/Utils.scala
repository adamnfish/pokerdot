package io.adamnfish.pokerdot.logic

import io.adamnfish.pokerdot.models.{Attempt, Failure, Failures}
import zio.IO


object Utils {
  implicit class RichList[A](as: List[A]) {
    def eTraverse[L, R](f: A => Either[L, R]): Either[L, List[R]] = {
      as.foldRight[Either[L, List[R]]](Right(Nil)) { (a, eAcc) =>
        for {
          r <- f(a)
          acc <- eAcc
        } yield r :: acc
      }
    }

    def ioTraverse[B](f: A => Attempt[B]): Attempt[List[B]] = {
      as.foldRight[Attempt[List[B]]](IO.succeed(Nil))((a, acc) => IO.mapParN(f(a), acc)(_ :: _))
    }
  }

  implicit class RichAttempt[A](attempt: Attempt[A]) {
    def |!|(attempt2: Attempt[A]): Attempt[Unit] = {
      IO.partition(List(attempt, attempt2))(identity).flatMap { case (failedResults, _) =>
        if (failedResults.isEmpty) {
          IO.unit
        } else {
          val allFailures = failedResults.foldLeft[List[Failure]](Nil) { case (acc, failures) =>
            acc ++ failures.failures
          }
          IO.fail(Failures(allFailures))
        }
      }
    }
  }

  implicit class RichEither[A](efa: Either[Failures, A]) {
    def attempt: Attempt[A] = {
      IO.fromEither(efa)
    }
  }

  object Attempt {
    def failAs[A](failures: Failures): Attempt[A] = {
      val failed: Attempt[A] = IO.fail(failures)
      failed
    }

    def fromOption[A](ao: Option[A], ifEmpty: Failures): Attempt[A] = {
      ao.fold[Attempt[A]](IO.fail(ifEmpty))(a => IO.succeed(a))
    }
  }
}
