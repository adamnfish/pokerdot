package io.adamnfish.pokerdot.validation

import io.adamnfish.pokerdot.models.{BreakLevel, Failure, RoundLevel, TimerLevel}


object Validators {
  // VALIDATORS
  type Validator[A] = (A, String, String) => List[Failure]

  def nonEmpty: Validator[String] = { (str, context, friendlyContext) =>
    if (str.isEmpty) {
      List(
        Failure("Validation failure: empty", s"$friendlyContext is required", Some(context))
      )
    } else Nil
  }

  def nonEmptyList[A]: Validator[List[A]] = { (as, context, friendlyContext) =>
    if (as.isEmpty) {
      List(
        Failure("Validation failure: empty", s"$friendlyContext is required", Some(context))
      )
    } else Nil
  }

  private val uuidPattern = "[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}".r
  val isUUID: Validator[String] = { (str, context, friendlyContext) =>
    val wasEmpty = nonEmpty(str, context, friendlyContext).headOption
    val wasUUID =
      if (uuidPattern.pattern.matcher(str).matches) {
        None
      } else {
        Some(
          Failure(s"Validation failure: $str not UUID", s"$friendlyContext was not in the correct format", Some(context))
        )
      }
    wasEmpty.orElse(wasUUID).toList
  }

  val positiveInteger: Validator[Int] = { (i, context, friendlyContext) =>
    if (i < 0) {
      List(
        Failure("Validation failure: empty", s"$friendlyContext must be a positive number", Some(context))
      )
    } else Nil
  }

  val greaterThanZero: Validator[Int] = { (i, context, friendlyContext) =>
    if (i <= 0) {
      List(
        Failure("Validation failure: empty", s"$friendlyContext must be a positive number", Some(context))
      )
    } else Nil
  }

  /**
   * Game codes are a case-insensitive UUID prefix
   *
   * Allows upper and lower "oh" o/O as a surrogate for 0
   */
  val gameCode: Validator[String] = { (str, context, friendlyContext) =>
    val wasTooShort = minLength(4)(str, context, friendlyContext).headOption
    val ValidChar = "([0-9a-fA-FoO\\-])".r
    val valid = str.zipWithIndex.forall {
      case (ValidChar(c), i) =>
        if (i == 8 || i == 13 || i == 18 || i == 23) {
          c == '-'
        } else true
      case _ =>
        false
    }
    val wasUUIDPrefix =
      if (valid) None
      else Some(Failure(s"$str is not a UUID prefix", s"invalid $friendlyContext", Some(context)))
    wasTooShort.orElse(wasUUIDPrefix).toList
  }

  def minLength(min: Int): Validator[String] = { (str, context, friendlyContext) =>
    if (str.length < min)
      List(
        Failure("Failed min length", s"$friendlyContext must be at least $min characters", Some(context))
      )
    else Nil
  }

  def maxLength(max: Int): Validator[String] = { (str, context, friendlyContext) =>
    if (str.length > max)
      List(
        Failure("Failed max length", s"$friendlyContext must be no more than $max characters", Some(context))
      )
    else Nil
  }

  def sensibleLength: Validator[String] = { (str, context, friendlyContext) =>
    nonEmpty(str, context, friendlyContext) ++ maxLength(50)(str, context, friendlyContext)
  }

  val sensibleDurationSeconds: Validator[Int] = { (timestamp, context, friendlyContext) =>
    val twoWeeksInSeconds = 86400 * 14
    if (timestamp > twoWeeksInSeconds)
      List(
        Failure("Failed sensible duration validation", s"$friendlyContext is too large to be a timer duration", Some(context))
      )
    else Nil
  }

  def timerLevel: Validator[TimerLevel] = { (tl, context, friendlyContext) =>
    tl match {
      case RoundLevel(duration, smallBlind) =>
        greaterThanZero(duration, context, friendlyContext) ++ greaterThanZero(smallBlind, context, friendlyContext)
      case BreakLevel(duration) =>
        greaterThanZero(duration, context, friendlyContext)
    }
  }
}

