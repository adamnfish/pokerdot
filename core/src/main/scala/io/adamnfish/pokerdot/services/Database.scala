package io.adamnfish.pokerdot.services

import io.adamnfish.pokerdot.models.{Attempt, GameDb, GameId, GameLogEntryDb, PlayerDb}

trait Database {
  def getGame(gameId: GameId): Attempt[Option[GameDb]]

  def lookupGame(gameCode: String): Attempt[Option[GameDb]]

  def searchGameCode(gameCode: String): Attempt[List[GameDb]]

  def getPlayers(gameId: GameId): Attempt[List[PlayerDb]]

  def writeGame(gameDB: GameDb): Attempt[Unit]

  def writePlayer(playerDB: PlayerDb): Attempt[Unit]
  def writePlayers(playerDBs: Set[PlayerDb]): Attempt[Unit]

  // get log entries only back as far as the start of this phase
  // this can use a smaller query on the assumption there won't be millions of matching entries
  def getPhaseGameLog(gameId: GameId): Attempt[List[GameLogEntryDb]]

  // get all log entries for this game
  // this is unlikely to be useful, we might have to paginate it?
  def getFullGameLog(gameId: GameId): Attempt[List[GameLogEntryDb]]

  def writeGameEvent(gameLogEntryDb: GameLogEntryDb): Attempt[Unit]
  def writeGameEvents(gameLogEntryDbs: Set[GameLogEntryDb]): Attempt[Unit]
}

object Database {
  def checkUniquePrefix(gameId: GameId, prefixLength: Int, persistence: Database): Attempt[Boolean] = {
    val gameCode = gameId.gid.take(prefixLength)
    for {
      gameDbs <- persistence.searchGameCode(gameCode)
    } yield gameDbs.isEmpty
  }
}
