package io.adamnfish.pokerdot.services

import io.adamnfish.pokerdot.models.{GameDb, GameId, PlayerAddress, PlayerDb, PlayerId}
import cats.Monad
import cats._
import cats.data._
import cats.syntax.all._


trait Database[F[_]] {
  def getGame(gameId: GameId): F[Option[GameDb]]

  def lookupGame(gameCode: String): F[Option[GameDb]]

  def searchGameCode(gameCode: String): F[List[GameDb]]

  def getPlayers(gameId: GameId): F[List[PlayerDb]]

  def writeGame(gameDB: GameDb): F[Unit]

  def writePlayer(playerDB: PlayerDb): F[Unit]

  /**
   * Updates only the player's address, so this cannot overwrite gameplay
   * changes made by a concurrent request. Fails if the player does not exist.
   */
  def updatePlayerAddress(gameId: GameId, playerId: PlayerId, playerAddress: PlayerAddress): F[Unit]
}

object Database {
  def checkUniquePrefix[F[_] : Monad](gameId: GameId, prefixLength: Int, persistence: Database[F]): F[Boolean] = {
    val gameCode = gameId.gid.take(prefixLength)
    for {
      gameDbs <- persistence.searchGameCode(gameCode)
    } yield gameDbs.isEmpty
  }
}
