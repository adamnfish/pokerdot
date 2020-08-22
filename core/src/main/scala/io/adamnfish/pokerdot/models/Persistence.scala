package io.adamnfish.pokerdot.models

import java.time.ZonedDateTime


case class GameDb(
  gameCode: String, // partition
  gameId: String,   // sort
  gameName: String,
  playerIds: List[String],
  spectatorIds: List[String],
  seed: Long,
  phase: Phase,
  inTurn: Option[String],
  button: Int,
  started: Boolean,
  startTime: ZonedDateTime,
  expiry: Long,
  trackStacks: Boolean,
  timer: Option[TimerStatus],
)

case class PlayerDb(
  gameId: String,   // partition
  playerId: String, // sort
  playerAddress: String,
  playerKey: String,
  screenName: String,
  stack: Int,
  pot: Int,
  bid: Int,
  folded: Boolean,
  busted: Boolean,
  hole: Option[Hole],
  isCreator: Boolean,
)

case class SpectatorDb(
  gameId: String,   // partition
  playerId: String, // sort
  playerAddress: String,
  playerKey: String,
)
