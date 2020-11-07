package io.adamnfish.pokerdot.models


case class GameSummary(
  gameId: GameId,
  gameName: String,
  players: List[PlayerSummary],
  spectators: List[SpectatorSummary],
  round: RoundSummary,
  inTurn: Option[PlayerSummary],
  button: Int,
  started: Boolean,
  startTime: Long,
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

sealed trait Self

case class SpectatorSummary(
  playerId: PlayerId,
  screenName: String,
) extends Self

case class SelfSummary(
  playerId: PlayerId,
  screenName: String,
  stack: Int,
  pot: Int,
  bid: Int,
  folded: Boolean,
  busted: Boolean,
  hole: Option[Hole],
) extends Self

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
  holes: List[(PlayerId, Hole)],
) extends RoundSummary


sealed trait ActionSummary extends Product
case class PlayerJoinedSummary(
  player: PlayerSummary,
) extends ActionSummary
case class BetSummary(
  player: PlayerSummary,
) extends ActionSummary
case class CheckSummary(
  player: PlayerSummary,
) extends ActionSummary
case class FoldSummary(
  player: PlayerSummary,
) extends ActionSummary
case class AdvancePhaseSummary(
) extends ActionSummary
case class NoActionSummary(
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
  startingStack: Option[Int],
  timerConfig: List[TimerLevel],
  playerOrder: List[PlayerId],
) extends Request
case class UpdateTimer(
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
  spectator: Boolean,
) extends Message
case class GameStatus(
  self: Self,
  game: GameSummary,
  action: ActionSummary,
) extends Message
case class RoundWinnings(
  self: Self,
  game: GameSummary,
  pots: List[PotWinnings],
  players: List[PlayerWinnings],
) extends Message
case class Status(
  message: String
) extends Message
