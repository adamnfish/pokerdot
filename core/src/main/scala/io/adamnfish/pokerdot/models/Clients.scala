package io.adamnfish.pokerdot.models


case class GameSummary(
  gameId: GameId,
  gameCode: String,
  gameName: String,
  players: List[PlayerSummary],
  spectators: List[SpectatorSummary],
  round: RoundSummary,
  smallBlind: Int,
  inTurn: Option[PlayerId],
  button: Int,
  started: Boolean,
  startTime: Long,
  trackStacks: Boolean,
  timer: Option[TimerStatus],
)

case class PlayerSummary(
  playerId: PlayerId,
  screenName: String,
  stack: Int,
  pot: Int,
  bet: Int,
  folded: Boolean,
  busted: Boolean,
  isHost: Boolean,
  isAdmin: Boolean,
  hole: Option[Hole],
)

sealed trait Self

case class SpectatorSummary(
  playerId: PlayerId,
  screenName: String,
  isHost: Boolean,
  isAdmin: Boolean,
) extends Self

case class SelfSummary(
  playerId: PlayerId,
  screenName: String,
  stack: Int,
  pot: Int,
  bet: Int,
  folded: Boolean,
  busted: Boolean,
  hole: Option[Hole],
  isHost: Boolean,
  isAdmin: Boolean,
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
case class GameStartedSummary(
) extends ActionSummary
case class PlayerJoinedSummary(
  playerId: PlayerId,
) extends ActionSummary
case class CallSummary(
  playerId: PlayerId,
) extends ActionSummary
case class BetSummary(
  playerId: PlayerId,
  bet: Int,
) extends ActionSummary
case class CheckSummary(
  playerId: PlayerId,
) extends ActionSummary
case class FoldSummary(
  playerId: PlayerId,
) extends ActionSummary
case class AdvancePhaseSummary(
) extends ActionSummary
case class TimerStatusSummary(
  playing: Boolean,
) extends ActionSummary
case class EditTimerSummary(
) extends ActionSummary
case class EditBlindSummary(
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
  initialSmallBlind: Option[Int],
  timerConfig: Option[List[TimerLevel]],
  playerOrder: List[PlayerId],
) extends Request
case class UpdateBlind(
  gameId: GameId,
  playerId: PlayerId,
  playerKey: PlayerKey,
  timerLevels: Option[List[TimerLevel]],
  smallBlind: Option[Int],
  playing: Option[Boolean], // TODO: make this optional?
) extends Request
// game requests
case class Bet(
  gameId: GameId,
  playerKey: PlayerKey,
  playerId: PlayerId,
  betAmount: Int,
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
  // TODO: option to deal all cards and advance straight to showdown
) extends Request
// implementation detail requests
case class Ping(
  gameId: GameId,
  playerId: PlayerId,
  playerKey: PlayerKey,
) extends Request
case class Wake(
) extends Request

// Variance is required because advancePhase returns different messages depending on the phase
// this my be a sign that the advancePhase endpoint should be split up.
// However, with features like "auto-advance" this may be required in the future, so it isn't
// worth refactoring around this requirement for now.
case class Response[+M <: Message](
  messages: Map[PlayerAddress, M],
  statuses: Map[PlayerAddress, GameStatus],
)

// Data sent to clients
// TODO: consider a way to show hole cards during all-in when players can no longer act
//       actually, this is already in the round summary for showdown, so we should add it there for other phases
sealed trait Message extends Product
case class Welcome(
  playerKey: PlayerKey,
  playerId: PlayerId,
  gameId: GameId,
  gameCode: String,
  gameName: String,
  screenName: String,
  spectator: Boolean,
  game: GameSummary,
  self: SelfSummary,
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
