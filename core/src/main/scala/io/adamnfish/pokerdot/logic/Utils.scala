package io.adamnfish.pokerdot.logic

import io.adamnfish.pokerdot.models.{Attempt, Failure, Failures}
import zio.IO


object Utils {
  implicit class RichList[A](val as: List[A]) extends AnyVal {
    def eTraverse[L, R](f: A => Either[L, R]): Either[L, List[R]] = {
      as.foldRight[Either[L, List[R]]](Right(Nil)) { (a, eAcc) =>
        for {
          r <- f(a)
          acc <- eAcc
        } yield r :: acc
      }
    }

    def ioTraverse[B](f: A => Attempt[B]): Attempt[List[B]] = {
      IO.validatePar(as)(f).mapError { fss =>
        // collect failure instances from multiple failures into a single Failures instance
        Failures(fss.flatMap(_.failures))
      }
    }

    /**
     * Converts from stdlib's `-1 = empty` to an Option
     */
    def findIndex(p: A => Boolean): Option[Int] = {
      val i = as.indexWhere(p)
      if (i == -1) None
      else Some(i)
    }
  }

  implicit class RichAttempt[A](val attempt: Attempt[A]) extends AnyVal {
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

  implicit class RichFailureList(val failures: List[Failure]) extends AnyVal {
    def |!|(otherFailures: List[Failure]): List[Failure] = {
      failures ++ otherFailures
    }
  }

  implicit class RichEither[A](val efa: Either[Failures, A]) extends AnyVal {
    def attempt: Attempt[A] = {
      IO.fromEither(efa)
    }
  }

  object EitherUtils {
    def sequence[A](aes: List[Either[Failures, A]]): Either[Failures, List[A]] = {
      aes.foldRight[Either[Failures, List[A]]](Right(Nil)) { (ae, acc) =>
        acc match {
          case Left(accFailures) =>
            ae match {
              case Left(fs) =>
                Left(Failures(fs.failures ++ accFailures.failures))
              case Right(_) =>
                Left(accFailures)
            }
          case Right(as) =>
            ae.map(_ :: as)
        }
      }
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

  def orderFromList[A, B](original: List[A], order: List[B])(identify: A => B): List[A] = {
    original.sortBy { a =>
      val aId = identify(a)
      val index = order.indexOf(aId)
      if (index == -1) original.length
      else index
    }
  }
}
