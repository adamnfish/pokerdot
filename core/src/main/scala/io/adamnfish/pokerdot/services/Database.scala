package io.adamnfish.pokerdot.services

import io.adamnfish.pokerdot.models.{Attempt, GameDb, GameId, PlayerDb}

trait Database {
  def getGame(gameId: GameId): Attempt[Option[GameDb]]

  def lookupGame(gameCode: String): Attempt[Option[GameDb]]

  def searchGameCode(gameCode: String): Attempt[List[GameDb]]

  def getPlayers(gameId: GameId): Attempt[List[PlayerDb]]

  def writeGame(gameDB: GameDb): Attempt[Unit]

  def writePlayer(playerDB: PlayerDb): Attempt[Unit]
}

object Database {
  def checkUniquePrefix(gameId: GameId, prefixLength: Int, persistence: Database): Attempt[Boolean] = {
    val gameCode = gameId.gid.take(prefixLength)
    for {
      gameDbs <- persistence.searchGameCode(gameCode)
    } yield gameDbs.isEmpty
  }
}
