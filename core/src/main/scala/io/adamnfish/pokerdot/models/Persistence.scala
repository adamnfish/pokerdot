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

// this table is persisted, so let's use more compact keys
case class GameLogEntryDb(
  gid: String,  // partition
  ctd: Long,    // sort
  e: GameEventDb,
)
sealed trait GameEventDb extends Product with Serializable
case class GS(
  ps: List[String],
) extends GameEventDb
case class NR(
  s: Long,
  b: Int,
  sb: Option[Int],
  sp: Option[String],
  bp: String,
  ps: List[Int],
) extends GameEventDb
case class AR(
) extends GameEventDb
case class NP(
  p: String,
) extends GameEventDb
case class C(
  p: String,
) extends GameEventDb
case class B(
  p: String,
  b: Int,
) extends GameEventDb
case class F(
  p: String,
) extends GameEventDb
case class GE(
  w: String,
) extends GameEventDb
