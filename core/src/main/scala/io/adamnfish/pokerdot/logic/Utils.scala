package io.adamnfish.pokerdot.logic

import io.adamnfish.pokerdot.models.{Failure, Failures}


object Utils {
  implicit class RichList[A](val as: List[A]) extends AnyVal {
    /**
     * Converts from stdlib's `-1 = empty` to an Option
     */
    def findIndex(p: A => Boolean): Option[Int] = {
      val i = as.indexWhere(p)
      if (i == -1) None
      else Some(i)
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
