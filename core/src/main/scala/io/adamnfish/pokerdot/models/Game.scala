package io.adamnfish.pokerdot.models

import java.time.ZonedDateTime


case class Game(
  gameId: GameId,
  gameName: String,
  players: List[Player],
  spectators: List[Spectator],
  seed: Long,
  round: Round,
  inTurn: Option[Player],
  button: Int,
  started: Boolean,
  startTime: ZonedDateTime,
  expiry: Long,
  trackStacks: Boolean,
  timer: Option[TimerStatus]
)

case class Player(
  gameId: GameId,
  playerId: PlayerId,
  playerAddress: PlayerAddress,
  playerKey: PlayerKey,
  screenName: String,
  stack: Int,
  pot: Int,
  bid: Int,
  folded: Boolean,
  busted: Boolean,
  hole: Option[Hole],
  isCreator: Boolean,
)

/**
 * Spectators are not part of the game and do not actually play Poker.
 * However they do have keys and can make non-game requests:
 *   - advancing the round
 *   - updating the timer
 *
 * As well as being people watching the game, spectators can be "second screens",
 * e.g. providing a UI for the timer or showing the community cards.
 */
case class Spectator(
  gameId: GameId,
  playerId: PlayerId,
  playerAddress: PlayerAddress,
  playerKey: PlayerKey,
)

case class GameId(gid: String)
case class PlayerId(pid: String)
case class PlayerAddress(address: String)
case class PlayerKey(key: String)
