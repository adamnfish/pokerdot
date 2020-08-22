package io.adamnfish.pokerdot.models


case class Failure(
  logMessage: String,
  userMessage: String,
  context: Option[String],
)
