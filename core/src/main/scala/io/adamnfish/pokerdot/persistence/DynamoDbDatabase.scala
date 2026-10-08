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
import software.amazon.awssdk.services.dynamodb.model.{
  AttributeValue,
  ConditionCheck,
  ConditionalCheckFailedException,
  Put,
  TransactWriteItem,
  TransactWriteItemsRequest,
  TransactionCanceledException,
  Update
}

import java.util.concurrent.CompletionException
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*
import scala.util.Random
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
          games.consistently.get("gameCode" === gameCode and "gameId" === gameId.gid)
        )
      )
      maybeGameDb <- maybeResult.fold[F[Option[GameDb]]](Async[F].pure(None)) {
        result =>
          handleDbReadErr(result).map(Some(_))
      }
    } yield maybeGameDb

  }

  override def lookupGame(gameCode: String): F[Option[GameDb]] = {
    if (gameCode.isEmpty)
      Async[F].raiseError(
        Failures(
          "empty gameCode provided to searchGameCode",
          "error fetching saved data",
          exception = None
        )
      )
    else {
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
  }

  override def searchGameCode(gameCode: String): F[List[GameDb]] = {
    if (gameCode.isEmpty)
      Async[F].raiseError(
        Failures(
          "empty gameCode provided to searchGameCode",
          "error fetching saved data",
          exception = None
        )
      )
    else {
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
  }

  override def getPlayers(gameId: GameId): F[List[PlayerDb]] = {
    for {
      results <- handleDbErr(
        scanamo.exec(players.consistently.query("gameId" === gameId.gid))
      )
      players <- results.traverse(handleDbReadErr)
    } yield players
  }

  override def createGame(gameDb: GameDb, playerDb: PlayerDb): F[Unit] = {
    runTransaction(
      List(
        TransactWriteItem.builder().put(
          Put.builder()
            .tableName(gameTableName)
            .item(DynamoFormat[GameDb].write(gameDb).asObject.get.toJavaMap)
            .conditionExpression("attribute_not_exists(#gameId)")
            .expressionAttributeNames(Map("#gameId" -> "gameId").asJava)
            .build()
        ).build(),
        putNewPlayer(playerDb),
      )
    ) {
      case 0 =>
        Failures(s"Game ${gameDb.gameId} already exists", "couldn't create the game, please try again.")
      case _ =>
        Failures(s"Player ${playerDb.playerId} already exists", "couldn't create the game, please try again.")
    }
  }

  override def addPlayer(readGameDb: GameDb, playerDb: PlayerDb): F[Unit] = {
    runTransaction(
      List(
        TransactWriteItem.builder().conditionCheck(
          ConditionCheck.builder()
            .tableName(gameTableName)
            .key(gameKey(readGameDb))
            .conditionExpression("#revision = :revision")
            .expressionAttributeNames(Map("#revision" -> "revision").asJava)
            .expressionAttributeValues(Map(":revision" -> revisionValue(readGameDb.revision)).asJava)
            .build()
        ).build(),
        putNewPlayer(playerDb),
      )
    ) {
      case 0 =>
        Failures(
          s"Game ${readGameDb.gameId} has changed since revision ${readGameDb.revision}, cannot add player",
          "the game changed while you were joining, please try again.",
        )
      case _ =>
        Failures(s"Player ${playerDb.playerId} already exists", "couldn't join the game, please try again.")
    }
  }

  override def writeGameAndPlayers(gameDb: GameDb, playerDbs: List[PlayerDb]): F[Unit] = {
    val newGameDb = gameDb.copy(revision = gameDb.revision + 1)
    val gameItem = TransactWriteItem.builder().put(
      Put.builder()
        .tableName(gameTableName)
        .item(DynamoFormat[GameDb].write(newGameDb).asObject.get.toJavaMap)
        .conditionExpression("#revision = :revision")
        .expressionAttributeNames(Map("#revision" -> "revision").asJava)
        .expressionAttributeValues(Map(":revision" -> revisionValue(gameDb.revision)).asJava)
        .build()
    ).build()
    runTransaction(gameItem :: playerDbs.map(updatePlayerGameplay)) {
      case 0 =>
        Failures(
          s"Game ${gameDb.gameId} has changed since revision ${gameDb.revision}",
          "someone else changed the game at the same time, please check and try again.",
        )
      case i =>
        Failures(
          s"Player ${playerDbs.lift(i - 1).fold("<unknown>")(_.playerId)} does not exist in game ${gameDb.gameId}",
          "there was a problem trying to save a user that could not be found.",
        )
    }
  }

  override def updatePlayerAddress(
      gameId: GameId,
      playerId: PlayerId,
      playerAddress: PlayerAddress
  ): F[PlayerDb] = {
    for {
      result <- handleDbErr(
        scanamo.exec(
          players
            .when(attributeExists("playerId"))
            .update(
              "gameId" === gameId.gid and "playerId" === playerId.pid,
              set("playerAddress", playerAddress.address)
            )
        )
      )
      playerDb <- handleConditionalWriteErr(result) { e =>
        Failures(
          s"Cannot update address for player ${playerId.pid} that does not exist in game ${gameId.gid}",
          "couldn't find you in the game.",
          exception = Some(e)
        )
      }
    } yield playerDb
  }

  private def putNewPlayer(playerDb: PlayerDb): TransactWriteItem = {
    TransactWriteItem.builder().put(
      Put.builder()
        .tableName(playerTableName)
        .item(DynamoFormat[PlayerDb].write(playerDb).asObject.get.toJavaMap)
        .conditionExpression("attribute_not_exists(#playerId)")
        .expressionAttributeNames(Map("#playerId" -> "playerId").asJava)
        .build()
    ).build()
  }

  /**
   * Writes only the fields that gameplay owns, so that identity fields like
   * the player's address (which is updated independently by pings) are untouched.
   */
  private def updatePlayerGameplay(playerDb: PlayerDb): TransactWriteItem = {
    val attributes = DynamoFormat[PlayerDb].write(playerDb).asObject.get.toJavaMap.asScala
    val (present, absent) = DynamoDbDatabase.playerGameplayFields.partition { field =>
      attributes.get(field).exists(av => !Option(av.nul()).contains(true))
    }
    val updateExpression = List(
      Option.when(present.nonEmpty)(present.map(field => s"#$field = :$field").mkString("SET ", ", ", "")),
      Option.when(absent.nonEmpty)(absent.map(field => s"#$field").mkString("REMOVE ", ", ", "")),
    ).flatten.mkString(" ")
    val update = Update.builder()
      .tableName(playerTableName)
      .key(Map(
        "gameId" -> AttributeValue.fromS(playerDb.gameId),
        "playerId" -> AttributeValue.fromS(playerDb.playerId),
      ).asJava)
      .updateExpression(updateExpression)
      .conditionExpression("attribute_exists(#playerId)")
      .expressionAttributeNames(
        (("playerId" :: DynamoDbDatabase.playerGameplayFields).map(field => s"#$field" -> field).toMap).asJava
      )
    val withValues =
      if (present.isEmpty) update
      else update.expressionAttributeValues(present.map(field => s":$field" -> attributes(field)).toMap.asJava)
    TransactWriteItem.builder().update(withValues.build()).build()
  }

  private def gameKey(gameDb: GameDb): java.util.Map[String, AttributeValue] = {
    Map(
      "gameCode" -> AttributeValue.fromS(gameDb.gameCode),
      "gameId" -> AttributeValue.fromS(gameDb.gameId),
    ).asJava
  }

  private def revisionValue(revision: Long): AttributeValue =
    AttributeValue.fromN(revision.toString)

  /**
   * Runs the transaction, retrying transient failures.
   *
   * If a condition fails, the index of the failed item is passed to `conditionFailed`
   * to describe the problem. These failures are not retried, the request was based
   * on stale data so it may no longer be valid.
   */
  private def runTransaction(items: List[TransactWriteItem], attempt: Int = 1)(conditionFailed: Int => Failures): F[Unit] = {
    val request = TransactWriteItemsRequest.builder().transactItems(items.asJava).build()
    handleDbErr {
      Async[F].fromCompletableFuture(Async[F].delay(client.transactWriteItems(request))).void
        .adaptError { case e: CompletionException if e.getCause != null => e.getCause }
        .handleErrorWith {
          case tce: TransactionCanceledException =>
            val reasonCodes = Option(tce.cancellationReasons()).map(_.asScala.toList).getOrElse(Nil).map(_.code())
            reasonCodes.indexOf("ConditionalCheckFailed") match {
              case -1 if reasonCodes.exists(DynamoDbDatabase.transientReasons.contains) && attempt < DynamoDbDatabase.maxAttempts =>
                val backoff = (50 * attempt + Random.nextInt(50)).millis
                Async[F].sleep(backoff) >> runTransaction(items, attempt + 1)(conditionFailed)
              case -1 =>
                Async[F].raiseError(Failures(
                  s"DynamoDB transaction cancelled after $attempt attempt(s), reasons: ${reasonCodes.mkString(", ")}",
                  "error saving data",
                  exception = Some(tce),
                ))
              case failedIndex =>
                Async[F].raiseError(conditionFailed(failedIndex))
            }
          case other =>
            Async[F].raiseError(other)
        }
    }
  }

  private def handleDbReadErr[A](
      result: Either[DynamoReadError, A]
  ): F[A] = {
    Async[F].fromEither {
      result.left.map(dynamoReadFailure)
    }
  }

  private def handleConditionalWriteErr[A](
      result: Either[ScanamoError, A]
  )(conditionNotMet: ConditionalCheckFailedException => Failures): F[A] = {
    Async[F].fromEither {
      result.left.map {
        case ConditionNotMet(e) =>
          conditionNotMet(e)
        case dre: DynamoReadError =>
          dynamoReadFailure(dre)
      }
    }
  }

  private def dynamoReadFailure(dre: DynamoReadError): Failures = {
    Failures(
      s"DynamoReadError: ${DynamoReadError.describe(dre)}",
      "error reading saved data",
      exception = dre match {
        case TypeCoercionError(t) => Some(t)
        case _ => None
      }
    )
  }

  private def handleDbErr[A](fa: F[A]): F[A] =
    Async[F].adaptError(fa) {
      // already a meaningful error
      case failures: Failures => failures
      case NonFatal(err) =>
        Failures(
          "unhandled DynamoDB error",
          "error fetching saved data",
          exception = Some(err)
        )
    }
}

object DynamoDbDatabase {
  /**
   * The player fields that change during gameplay.
   * Everything else on a PlayerDb is identity, set when the player joins.
   */
  val playerGameplayFields: List[String] =
    List("stack", "pot", "bet", "checked", "folded", "busted", "hole", "holeVisible", "blind")

  private val maxAttempts = 3
  private val transientReasons = Set("TransactionConflict", "ThrottlingError")
}
