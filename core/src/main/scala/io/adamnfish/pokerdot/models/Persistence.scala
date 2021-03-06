package io.adamnfish.pokerdot.models


case class GameDb(
  gameCode: String, // partition
  gameId: String,   // sort
  expiry: Long,
  gameName: String,
  playerIds: List[String],
  spectatorIds: List[String],
  seed: Long,
  phase: Phase,
  smallBlind: Int,
  inTurn: Option[String],
  button: Int,
  started: Boolean,
  startTime: Long,
  trackStacks: Boolean,
  timer: Option[TimerStatus],
)

case class PlayerDb(
  gameId: String,   // partition
  playerId: String, // sort
  expiry: Long,
  playerAddress: String,
  playerKey: String,
  screenName: String,
  stack: Int,
  pot: Int,
  bet: Int,
  checked: Boolean,
  folded: Boolean,
  busted: Boolean,
  hole: Option[Hole],
  holeVisible: Boolean,
  isHost: Boolean,
  isAdmin: Boolean,
  blind: Int,
  isSpectator: Boolean,
)
