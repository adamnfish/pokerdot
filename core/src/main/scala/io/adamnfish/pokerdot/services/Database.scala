package io.adamnfish.pokerdot.services

import io.adamnfish.pokerdot.models.{ConnectionDb, GameDb, GameId, PlayerDb}
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

  def putConnection(connection: ConnectionDb): F[Unit]

  def getConnections(gameId: GameId): F[List[ConnectionDb]]
}

object Database {
  def checkUniquePrefix[F[_] : Monad](gameId: GameId, prefixLength: Int, persistence: Database[F]): F[Boolean] = {
    val gameCode = gameId.gid.take(prefixLength)
    for {
      gameDbs <- persistence.searchGameCode(gameCode)
    } yield gameDbs.isEmpty
  }
}
