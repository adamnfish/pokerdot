package io.adamnfish.pokerdot.services

import io.adamnfish.pokerdot.models.{ConnectionDb, GameDb, GameId, PlayerAddress, PlayerDb}
import cats.Monad
import cats._
import cats.data._
import cats.syntax.all._


/**
 * Game writes are conditional on the revision of the game that was read, so a write based on stale data fails.
 * Handlers always read the game before its players, and players are only written alongside the game,
 * so the game's revision check also protects the players.
 */
trait Database[F[_]] {
  def getGame(gameId: GameId): F[Option[GameDb]]

  def lookupGame(gameCode: String): F[Option[GameDb]]

  def searchGameCode(gameCode: String): F[List[GameDb]]

  def getPlayers(gameId: GameId): F[List[PlayerDb]]

  def createGame(gameDb: GameDb, playerDb: PlayerDb, connection: ConnectionDb): F[Unit]

  /**
   * Fails if the game has changed since it was read, i.e. it has started.
   */
  def addPlayer(readGame: GameDb, playerDb: PlayerDb, connection: ConnectionDb): F[Unit]

  /**
   * Writes the new game and the players atomically, if the game is still at the read game's revision.
   */
  def writeGame(readGame: GameDb, newGame: GameDb, players: List[PlayerDb]): F[Unit]

  def putConnection(connection: ConnectionDb): F[Unit]

  def getConnections(gameId: GameId): F[List[ConnectionDb]]

  def removeConnection(gameId: GameId, address: PlayerAddress): F[Unit]
}

object Database {
  def checkUniquePrefix[F[_] : Monad](gameId: GameId, prefixLength: Int, persistence: Database[F]): F[Boolean] = {
    val gameCode = gameId.gid.take(prefixLength)
    for {
      gameDbs <- persistence.searchGameCode(gameCode)
    } yield gameDbs.isEmpty
  }
}
