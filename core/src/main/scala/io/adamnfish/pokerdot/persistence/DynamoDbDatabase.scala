package io.adamnfish.pokerdot.persistence

import software.amazon.awssdk.services.dynamodb.DynamoDbClient
import io.adamnfish.pokerdot.logic.Games
import io.adamnfish.pokerdot.logic.Utils.{EitherUtils, RichList}
import io.adamnfish.pokerdot.models.{Attempt, Failure, Failures, GameDb, GameId, PlayerDb}
import io.adamnfish.pokerdot.services.Database
import org.scanamo._
import org.scanamo.syntax._
import org.scanamo.generic.auto._
import zio.IO


class DynamoDbDatabase(client: DynamoDbClient, gameTableName: String, playerTableName: String) extends Database {
  // TODO: switch DB models to use PlayerId?
  //  provide implicit to allow Scanamo to use those wrapper types

  private val games = Table[GameDb](gameTableName)
  private val players = Table[PlayerDb](playerTableName)

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
            "Couldn't find a game for that code",
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
    for {
      result <- execAsAttempt(games.put(gameDB))
    } yield result
  }

  override def writePlayer(playerDB: PlayerDb): Attempt[Unit] = {
    for {
      result <- execAsAttempt(players.put(playerDB))
    } yield result
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
        Failures(s"DynamoReadError: $dre", "Error with saved data", None, None)
      }
    }
  }
}
