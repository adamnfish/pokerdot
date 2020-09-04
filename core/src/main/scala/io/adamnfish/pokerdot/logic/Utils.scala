package io.adamnfish.pokerdot.logic

import io.adamnfish.pokerdot.models.{Attempt, Failures}
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

  object Attempt {
    def failAs[A](failures: Failures): Attempt[A] = {
      val failed: Attempt[A] = IO.fail(failures)
      failed
    }
  }
}
