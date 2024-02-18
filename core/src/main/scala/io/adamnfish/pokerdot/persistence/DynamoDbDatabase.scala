package io.adamnfish.pokerdot.persistence

import cats.*
import cats.effect.Async
import cats.implicits.*
import cats.syntax.all.*
import io.adamnfish.pokerdot.logic.Games
import io.adamnfish.pokerdot.models.*
import io.adamnfish.pokerdot.services.Database
import org.scanamo.*
import org.scanamo.generic.auto.*
import org.scanamo.syntax.*
import software.amazon.awssdk.services.dynamodb.DynamoDbAsyncClient

import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

class DynamoDbDatabase[F[_]: Async](
    client: DynamoDbAsyncClient,
    gameTableName: String,
    playerTableName: String
) extends Database[F] {
  private val scanamo = ScanamoCats[F](client)
  // TODO: switch DB models to use PlayerId?
  //  provide implicit to allow Scanamo to use those wrapper types

  private val games = Table[GameDb](gameTableName)
  private val players = Table[PlayerDb](playerTableName)

  // TODO: consider whether this should just derive a gameCode and call lookup
  override def getGame(gameId: GameId): F[Option[GameDb]] = {
    val gameCode = Games.gameCode(gameId)
    for {
      maybeResult <- handleDbErr(
        scanamo.exec[Option[Either[DynamoReadError, GameDb]]](
          games.get("gameCode" === gameCode and "gameId" === gameId.gid)
        )
      )
      maybeGameDb <- maybeResult.fold[F[Option[GameDb]]](Async[F].pure(None)) {
        result =>
          handleDbReadErr(result).map(Some(_))
      }
    } yield maybeGameDb

  }

  override def lookupGame(gameCode: String): F[Option[GameDb]] = {
    for {
      results <- handleDbErr(
        scanamo.exec(
          games.query(
            "gameCode" === gameCode and ("gameId" beginsWith gameCode)
          )
        )
      )
      maybeResult <- results match {
        case Nil =>
          Async[F].pure(None)
        case result :: Nil =>
          Async[F].pure(Some(result))
        case _ =>
          Async[F].raiseError(
            Failure(
              s"Multiple games found for code `$gameCode`",
              "couldn't find a game for that code"
            ).asFailures
          )
      }
      maybeGameDb <- maybeResult.fold[F[Option[GameDb]]](
        Async[F].pure(None)
      ) { result =>
        handleDbReadErr(result).map(Some(_))
      }
    } yield maybeGameDb
  }

  override def searchGameCode(gameCode: String): F[List[GameDb]] = {
    for {
      results <- handleDbErr(
        scanamo.exec(
          games.query(
            "gameCode" === gameCode and ("gameId" beginsWith gameCode)
          )
        )
      )
      gameDbs <- results.traverse(handleDbReadErr)
    } yield gameDbs
  }

  override def getPlayers(gameId: GameId): F[List[PlayerDb]] = {
    for {
      results <- handleDbErr(
        scanamo.exec(players.query("gameId" === gameId.gid))
      )
      players <- results.traverse(handleDbReadErr)
    } yield players
  }

  override def writeGame(gameDB: GameDb): F[Unit] = {
    for {
      result <- handleDbErr(scanamo.exec(games.put(gameDB)))
    } yield result
  }

  override def writePlayer(playerDB: PlayerDb): F[Unit] = {
    for {
      result <- handleDbErr(scanamo.exec(players.put(playerDB)))
    } yield result
  }

  private def handleDbReadErr[A](
      result: Either[DynamoReadError, A]
  ): F[A] = {
    Async[F].fromEither {
      result.left.map { dre =>
        Failures(
          s"DynamoReadError: $dre",
          "error reading saved data",
          None,
          None
        )
      }
    }
  }

  private def handleDbErr[A](fa: F[A]): F[A] =
    Async[F].adaptError(fa) { case NonFatal(err) =>
      Failures(
        "unhandled DynamoDB error",
        "error fetching saved data",
        exception = Some(err)
      )
    }
}
