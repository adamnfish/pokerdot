package io.adamnfish.pokerdot.logic


object Utils {
  def eTraverse[A, L, R](as: List[A])(f: A => Either[L, R]): Either[L, List[R]] = {
    as.foldRight[Either[L, List[R]]](Right(Nil)) { (a, eAcc) =>
      for {
        r <- f(a)
        acc <- eAcc
      } yield r :: acc
    }
  }
}
