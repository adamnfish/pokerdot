package io.adamnfish.pokerdot.models


case class GameLogEntry(
  gameId: GameId,
  eventTime: Long,
  event: GameEvent,
)

sealed trait GameEvent extends Product
case class GameStartEvent(
  playerIds: List[PlayerId],
) extends GameEvent
case class NewRoundEvent(
  seed: Long,
  button: Int,
  smallBlind: Option[Int],
  smallBlindPlayer: Option[PlayerId],
  bigBlindPlayer: PlayerId,
  playerStacks: List[Int],
) extends GameEvent
case class AbandonRoundEvent(
) extends GameEvent
case class NewPhaseEvent(
  phase: Phase,
) extends GameEvent
case class CheckEvent(
  playerId: PlayerId,
) extends GameEvent
case class BetEvent(
  playerId: PlayerId,
  bet: Int,
) extends GameEvent
case class FoldEvent(
  playerId: PlayerId,
) extends GameEvent
case class GameEndEvent(
  winner: PlayerId,
) extends GameEvent

