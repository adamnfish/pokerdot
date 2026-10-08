package io.adamnfish.pokerdot.services

import io.adamnfish.pokerdot.models.{GameDb, GameId, PlayerAddress, PlayerDb, PlayerId}
import cats.Monad
import cats._
import cats.data._
import cats.syntax.all._


/**
 * Writes to a game are conditional on the game's revision, which is incremented by every write.
 * A write based on a stale read of the game fails rather than overwriting a concurrent change.
 *
 * The revision check is on the game record only, but it also protects the players' gameplay
 * fields because:
 * - callers always read the game *before* its players, using consistent reads
 * - gameplay fields are only ever written alongside the game, via `writeGameAndPlayers`
 * So a player's gameplay fields can't be newer than the game read before them unless the
 * game's revision has also moved on, which fails the write.
 *
 * Player addresses change independently (via `updatePlayerAddress`), and are never written
 * by gameplay, so they don't need the revision check.
 */
trait Database[F[_]] {
  def getGame(gameId: GameId): F[Option[GameDb]]

  def lookupGame(gameCode: String): F[Option[GameDb]]

  def searchGameCode(gameCode: String): F[List[GameDb]]

  def getPlayers(gameId: GameId): F[List[PlayerDb]]

  /**
   * Creates a new game along with its first player. Fails if the game already exists.
   */
  def createGame(gameDb: GameDb, playerDb: PlayerDb): F[Unit]

  /**
   * Adds a new player to the game, provided the game has not changed since it was read.
   * The game record itself is not written.
   */
  def addPlayer(readGameDb: GameDb, playerDb: PlayerDb): F[Unit]

  /**
   * Writes the game and the gameplay fields of the provided players, provided the game has
   * not changed since it was read.
   *
   * `gameDb.revision` must be the revision as read, this function increments it.
   * Player identity fields (address, key, screen name etc) are never written here.
   */
  def writeGameAndPlayers(gameDb: GameDb, playerDbs: List[PlayerDb]): F[Unit]

  def updatePlayerAddress(gameId: GameId, playerId: PlayerId, playerAddress: PlayerAddress): F[PlayerDb]
}

object Database {
  def checkUniquePrefix[F[_] : Monad](gameId: GameId, prefixLength: Int, persistence: Database[F]): F[Boolean] = {
    val gameCode = gameId.gid.take(prefixLength)
    for {
      gameDbs <- persistence.searchGameCode(gameCode)
    } yield gameDbs.isEmpty
  }
}
