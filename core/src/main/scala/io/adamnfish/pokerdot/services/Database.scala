package io.adamnfish.pokerdot.services

import io.adamnfish.pokerdot.models.{GameDb, GameId, PlayerAddress, PlayerDb, PlayerId}
import cats.Monad
import cats._
import cats.data._
import cats.syntax.all._


/**
 * Game writes are conditional on the revision that was read, so a write based on stale data fails.
 * This also covers the players' gameplay fields, because they are only written alongside the game,
 * and the game is always read before its players.
 */
trait Database[F[_]] {
  def getGame(gameId: GameId): F[Option[GameDb]]

  def lookupGame(gameCode: String): F[Option[GameDb]]

  def searchGameCode(gameCode: String): F[List[GameDb]]

  def getPlayers(gameId: GameId): F[List[PlayerDb]]

  def createGame(gameDb: GameDb, playerDb: PlayerDb): F[Unit]

  def addPlayer(readGameDb: GameDb, playerDb: PlayerDb): F[Unit]

  /**
   * `gameDb.revision` should be the revision that was read, this function increments it.
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
