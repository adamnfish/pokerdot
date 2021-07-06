package io.adamnfish.pokerdot.models

import zio.IO


case class Failures(failures: List[Failure]) {
  def logString: String = failures.map { failure =>
    List(
      Some(failure.logMessage),
      failure.context.map(c => s"context: $c"),
      failure.exception.map(e => "err: " + e.getStackTrace.mkString("\n")),
      failure.exception.flatMap(e => Option(e.getCause).map(c => "caused by: " + c.getStackTrace.mkString("\n")))
    ).flatten.mkString(" | ")
  }.mkString(", ")
}
object Failures {
  def apply(error: Failure): Failures = {
    Failures(List(error))
  }
  def apply(errors: Seq[Failure]): Failures = {
    Failures(errors.toList)
  }

  def apply(
    logMessage: String,
    userMessage: String,
    context: Option[String] = None,
    exception: Option[Throwable] = None
  ): Failures = {
    Failures(Failure(logMessage, userMessage, context, exception))
  }
}

// TODO: level (at least error and info)
case class Failure(
  logMessage: String,
  userMessage: String,
  context: Option[String] = None,
  exception: Option[Throwable] = None
) {
  def asIO: IO[Failures, Nothing] = IO.fail(Failures(this))
  def asFailures: Failures = Failures(this)
}
