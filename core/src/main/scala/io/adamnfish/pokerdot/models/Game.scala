package io.adamnfish.pokerdot.models

import java.time.ZonedDateTime
import io.adamnfish.pokerdot.services.{Database, Dates, Messaging, Rng}


case class Game(
  gameId: GameId,
  expiry: Long,
  gameName: String,
  players: List[Player],
  spectators: List[Spectator],
  seed: Long,
  round: Round,
  inTurn: Option[Player],
  button: Int, // 0-indexed dealer position
  started: Boolean,
  startTime: Long,
  trackStacks: Boolean,
  // autoAdvance: Boolean,
  timer: Option[TimerStatus]
)

case class Player(
  gameId: GameId,
  playerId: PlayerId,
  expiry: Long,
  playerAddress: PlayerAddress,
  playerKey: PlayerKey,
  screenName: String,
  stack: Int,
  pot: Int,
  bet: Int,
  checked: Boolean, // has the player checked at this bet amount
  folded: Boolean,
  busted: Boolean,
  hole: Option[Hole],
  isHost: Boolean,
)

/**
 * Spectators are not part of the game and do not actually play PokerHands.
 * However they do have keys and can make non-game requests:
 *   - advancing the round
 *   - updating the timer
 *
 * As well as being people watching the game, spectators can be "second screens",
 * e.g. providing a UI for the timer or showing the community cards.
 *
 * The host might be a spectator in a tournament setting, for example.
 */
case class Spectator(
  gameId: GameId,
  playerId: PlayerId,
  playerAddress: PlayerAddress,
  playerKey: PlayerKey,
  screenName: String,
  isHost: Boolean,
)

case class GameId(gid: String) extends AnyVal
case class PlayerId(pid: String) extends AnyVal
case class PlayerAddress(address: String) extends AnyVal
case class PlayerKey(key: String) extends AnyVal


case class AppContext(
  playerAddress: PlayerAddress,
  db: Database,
  messaging: Messaging,
  dates: Dates,
  rng: Rng,
)
