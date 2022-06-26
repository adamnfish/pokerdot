package io.adamnfish.pokerdot.models


case class GameLogEntry(
  gameId: GameId,
  eventTime: Long,
  event: GameEvent,
)

// is this useful, or should we just use the small one everywhere?
sealed trait GameEvent extends Product
case class GameStartEvent(
  playerIds: List[PlayerId],
) extends Product
case class NewRoundEvent(
  seed: Long,
  button: Int,
  smallBlind: Option[Int],
  sbPlayer: Option[PlayerId],
  bbPlayer: PlayerId,
) extends GameEvent
case class EndPhaseEvent(
  phase: Phase
) extends GameEvent
case class CheckEvent(
  playerId: String,
) extends GameEvent
case class BetEvent(
  playerId: String,
  bet: Int,
) extends GameEvent
case class FoldEvent(
  playerId: String,
) extends GameEvent
case class AbandonRoundEvent(
) extends GameEvent
case class GameEndEvent(
  winner: PlayerId,
) extends GameEvent
