package io.adamnfish.pokerdot.models


case class Failures private(
  failures: List[Failure], logString: String, exception: Option[Throwable]
) extends Throwable(logString, exception.orNull) {
  val externalFailures: List[Failure] = failures.filterNot(_.internal)
  
  def externalOnly: Failures = Failures(externalFailures)
}
object Failures {
  private def findException(failures: List[Failure]): Option[Throwable] =
    failures.find(_.exception.isDefined).flatMap(_.exception)

  private def generateLogString(failures: List[Failure]): String =
    failures.map { failure =>
      List(
        Some(failure.logMessage),
        failure.context.map(c => s"context: $c"),
        failure.exception.map(e => "err: " + e.getStackTrace.mkString("\n")),
        failure.exception.flatMap(e => Option(e.getCause).map(c => "caused by: " + c.getStackTrace.mkString("\n")))
      ).flatten.mkString(" | ")
    }.mkString(", ")

  def apply(error: Failure): Failures = {
    val failures = List(error)
    Failures(failures, generateLogString(failures), findException(failures))
  }

  def apply(failures: Failure*): Failures = {
    val lFailures = failures.toList
    new Failures(lFailures, generateLogString(lFailures), findException(lFailures))
  }

  def apply(failures: List[Failure]): Failures = {
    Failures(failures, generateLogString(failures), findException(failures))
  }

  def apply(
    logMessage: String,
    userMessage: String,
    context: Option[String] = None,
    exception: Option[Throwable] = None,
    internal: Boolean = false,
  ): Failures = {
    Failures(List(Failure(logMessage, userMessage, context, exception, internal)), logMessage, exception)
  }
}

// TODO: level (at least error and info)
case class Failure(
  logMessage: String,
  userMessage: String,
  context: Option[String] = None,
  exception: Option[Throwable] = None,
  internal: Boolean = false
) {
  def asFailures: Failures = Failures(this)
}
