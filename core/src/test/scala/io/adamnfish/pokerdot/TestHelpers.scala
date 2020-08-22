package io.adamnfish.pokerdot

import org.scalatest.matchers.HavePropertyMatcher
import org.scalatest.matchers.should.Matchers


trait TestHelpers extends Matchers {
  def having[A](propertyName: String, propertyValue: A): HavePropertyMatcher[AnyRef, Any] = {
    Symbol(propertyName) (propertyValue)
  }

  implicit class HavingTestHelperString(propertyName: String) {
    def as[A](propertyValue: A): HavePropertyMatcher[AnyRef, Any] = {
      Symbol(propertyName) (propertyValue)
    }
  }
}
