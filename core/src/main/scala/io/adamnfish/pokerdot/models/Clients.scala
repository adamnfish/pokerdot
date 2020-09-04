package io.adamnfish.pokerdot.models

import java.time.ZonedDateTime


case class GameSummary(
  gameId: GameId,
  gameName: String,
  players: List[PlayerSummary],
  spectators: List[SpectatorSummary],
  round: RoundSummary,
  inTurn: Option[PlayerSummary],
  button: Int,
  started: Boolean,
  startTime: ZonedDateTime,
  trackStacks: Boolean,
  timer: Option[TimerStatus]
)

case class PlayerSummary(
  playerId: PlayerId,
  screenName: String,
  stack: Int,
  pot: Int,
  bid: Int,
  folded: Boolean,
  busted: Boolean,
)

case class SpectatorSummary(
  playerId: PlayerId,
)

case class SelfSummary(
  playerId: PlayerId,
  screenName: String,
  stack: Int,
  pot: Int,
  bid: Int,
  folded: Boolean,
  busted: Boolean,
  hole: Option[Hole],
)

case class ResultSummary(
  player: PlayerSummary,
  hand: Hand,
  winnings: Int,
)


sealed trait RoundSummary extends Product
case class PreFlopSummary(
) extends RoundSummary
case class FlopSummary(
  flop1: Card,
  flop2: Card,
  flop3: Card,
) extends RoundSummary
case class TurnSummary(
  flop1: Card,
  flop2: Card,
  flop3: Card,
  turn: Card,
) extends RoundSummary
case class RiverSummary(
  flop1: Card,
  flop2: Card,
  flop3: Card,
  turn: Card,
  river: Card,
) extends RoundSummary
case class ShowdownSummary(
  flop1: Card,
  flop2: Card,
  flop3: Card,
  turn: Card,
  river: Card,
  hands: List[(PlayerId, Hole)],
) extends RoundSummary


sealed trait ActionSummary extends Product
case class PlayerJoinedSummary(
  playerSummary: PlayerSummary,
) extends ActionSummary
case class BetSummary(
  playerSummary: PlayerSummary,
) extends ActionSummary
case class CheckSummary(
  playerSummary: PlayerSummary,
) extends ActionSummary
case class FoldSummary(
  playerSummary: PlayerSummary,
) extends ActionSummary
case class AdvancePhaseSummary(
) extends ActionSummary


// Data received from clients
sealed trait Request extends Product
// game configuration requests
case class CreateGame(
  screenName: String,
  gameName: String,
) extends Request
case class JoinGame(
  gameCode: String,
  screenName: String,
) extends Request
case class StartGame(
  gameId: GameId,
  playerId: PlayerId,
  playerKey: PlayerKey,
  playerOrder: List[PlayerId],
) extends Request
case class UpdateTime(
  gameId: GameId,
  playerId: PlayerId,
  playerKey: PlayerKey,
  timerStatus: TimerStatus,
) extends Request
// game requests
case class Bid(
  gameId: GameId,
  playerKey: PlayerKey,
  playerId: PlayerId,
  bid: Int,
) extends Request
case class Check(
  gameId: GameId,
  playerKey: PlayerKey,
  playerId: PlayerId,
) extends Request
case class Fold(
  gameId: GameId,
  playerKey: PlayerKey,
  playerId: PlayerId,
) extends Request
case class AdvancePhase(
  gameId: GameId,
  playerKey: PlayerKey,
  playerId: PlayerId,
) extends Request
// implementation detail requests
case class Ping(
  gameId: GameId,
  playerId: PlayerId,
  playerKey: PlayerKey,
) extends Request
case class Wake(
) extends Request


case class Response[M <: Message](
  messages: Map[PlayerAddress, M],
  statuses: Map[PlayerAddress, GameStatus],
)

// Data sent to clients
sealed trait Message extends Product
case class Welcome(
  playerKey: PlayerKey,
  playerId: PlayerId,
  gameId: GameId,
  gameName: String,
  screenName: String,
) extends Message
case class GameStatus(
  self: SelfSummary,
  game: GameSummary,
  action: ActionSummary,
) extends Message
case class RoundWinnings(
  self: SelfSummary,
  game: GameSummary,
  results: List[ResultSummary],
) extends Message
case class Status(
  message: String
) extends Message
