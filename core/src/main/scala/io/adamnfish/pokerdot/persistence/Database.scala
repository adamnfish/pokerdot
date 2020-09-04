package io.adamnfish.pokerdot.persistence

import io.adamnfish.pokerdot.models.{Attempt, GameDb, GameId, PlayerDb}


trait Database {
  def getGame(gameId: GameId): Attempt[Option[GameDb]]

  def lookupGame(gameCode: String): Attempt[Option[GameDb]]

  def getPlayers(gameId: GameId): Attempt[List[PlayerDb]]

  def writeGame(gameDB: GameDb): Attempt[Unit]

  def writePlayer(playerDB: PlayerDb): Attempt[Unit]
}
