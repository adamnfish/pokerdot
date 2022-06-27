package io.adamnfish.pokerdot.persistence

import software.amazon.awssdk.services.dynamodb.DynamoDbClient
import io.adamnfish.pokerdot.logic.Games
import io.adamnfish.pokerdot.logic.Utils.{EitherUtils, RichList}
import io.adamnfish.pokerdot.models.{AR, Attempt, EP, Failure, Failures, GameDb, GameId, GameLogEntryDb, NR, PlayerDb}
import io.adamnfish.pokerdot.services.Database
import org.scanamo._
import org.scanamo.syntax._
import org.scanamo.generic.auto._
import zio.IO


class DynamoDbDatabase(client: DynamoDbClient, gameTableName: String, playerTableName: String, gameLogTableName: String) extends Database {
  // TODO: switch DB models to use PlayerId?
  //  provide implicit to allow Scanamo to use those wrapper types

  private val games = Table[GameDb](gameTableName)
  private val players = Table[PlayerDb](playerTableName)
  private val gameLogs = Table[GameLogEntryDb](gameLogTableName)

  // TODO: consider whether this should just derive a gameCode and call lookup
  override def getGame(gameId: GameId): Attempt[Option[GameDb]] = {
    val gameCode = Games.gameCode(gameId)
    for {
      maybeResult <- execAsAttempt(games.get("gameCode" === gameCode and "gameId" === gameId.gid))
      maybeGameDb <- maybeResult.fold[Attempt[Option[GameDb]]](IO.succeed(None)) { result =>
        resultToAttempt(result).map(Some(_))
      }
    } yield maybeGameDb
  }

  override def lookupGame(gameCode: String): Attempt[Option[GameDb]] = {
    for {
      results <- execAsAttempt(games.query("gameCode" === gameCode and ("gameId" beginsWith gameCode)))
      maybeResult <- results match {
        case Nil =>
          IO.succeed(None)
        case result :: Nil =>
          IO.succeed(Some(result))
        case _ =>
          Failure(
            s"Multiple games found for code `$gameCode`",
            "couldn't find a game for that code",
          ).asIO
      }
      maybeGameDb <- maybeResult.fold[Attempt[Option[GameDb]]](IO.succeed(None)) { result =>
        resultToAttempt(result).map(Some(_))
      }
    } yield maybeGameDb
  }

  override def searchGameCode(gameCode: String): Attempt[List[GameDb]] = {
    for {
      results <- execAsAttempt(games.query("gameCode" === gameCode and ("gameId" beginsWith gameCode)))
      gameDbs <- results.ioTraverse(resultToAttempt)
    } yield gameDbs
  }

  override def getPlayers(gameId: GameId): Attempt[List[PlayerDb]] = {
    for {
      results <- execAsAttempt(players.query("gameId" === gameId.gid))
      players <- results.ioTraverse(resultToAttempt)
    } yield players
  }

  override def writeGame(gameDB: GameDb): Attempt[Unit] = {
    execAsAttempt(games.put(gameDB))
  }

  override def writePlayer(playerDB: PlayerDb): Attempt[Unit] = {
    execAsAttempt(players.put(playerDB))
  }

  override def getPhaseGameLog(gameId: GameId): Attempt[List[GameLogEntryDb]] = {
    // TODO: think about order of records
    // TODO: start by querying smaller number of records, get more if needed
    for {
      results <- execAsAttempt(gameLogs.descending.query("gid" === gameId.gid))
      gameLogs <- results.ioTraverse(resultToAttempt)
    } yield gameLogs.takeWhile {
      _.e match {
        case _: EP => false
        case _: AR => false
        case _: NR => false
        case _ => true
      }
    }
  }

  override def getFullGameLog(gameId: GameId): Attempt[List[GameLogEntryDb]] = {
    for {
      results <- execAsAttempt(gameLogs.descending.query("gid" === gameId.gid))
      gameLogs <- results.ioTraverse(resultToAttempt)
    } yield gameLogs
  }

  override def writeGameEvent(gameLogEntryDb: GameLogEntryDb): Attempt[Unit] = {
    execAsAttempt(gameLogs.put(gameLogEntryDb))
  }

  def execAsAttempt[A](op: ops.ScanamoOps[A]): Attempt[A] = {
    IO.effect {
      Scanamo(client).exec(op)
    }.mapError { err =>
      Failures("Uncaught DB error", "I had a problem saving the game", None, Some(err))
    }
  }

  private def resultToAttempt[A](result: Either[DynamoReadError, A]): Attempt[A] = {
    IO.fromEither {
      result.left.map { dre =>
        Failures(s"DynamoReadError: $dre", "error with saved data", None, None)
      }
    }
  }
}
