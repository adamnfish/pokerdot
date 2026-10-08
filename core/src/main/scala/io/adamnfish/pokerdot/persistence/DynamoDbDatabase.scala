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
import org.scanamo.query.{ConditionExpression, UniqueKey}
import org.scanamo.syntax.*
import software.amazon.awssdk.services.dynamodb.DynamoDbAsyncClient
import software.amazon.awssdk.services.dynamodb.model.{
  ConditionCheck,
  Put,
  TransactWriteItem,
  TransactWriteItemsRequest,
  TransactionCanceledException
}

import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

class DynamoDbDatabase[F[_]: Async](
    client: DynamoDbAsyncClient,
    gameTableName: String,
    playerTableName: String,
    connectionTableName: String,
) extends Database[F] {
  private val scanamo = ScanamoCats[F](client)
  // TODO: switch DB models to use PlayerId?
  //  provide implicit to allow Scanamo to use those wrapper types

  private val games = Table[GameDb](gameTableName)
  private val players = Table[PlayerDb](playerTableName)
  private val connections = Table[ConnectionDb](connectionTableName)

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

  override def putConnection(connection: ConnectionDb): F[Unit] = {
    handleDbErr(scanamo.exec(connections.put(connection)))
  }

  override def getConnections(gameId: GameId): F[List[ConnectionDb]] = {
    for {
      results <- handleDbErr(
        scanamo.exec(connections.query("gameId" === gameId.gid))
      )
      connections <- results.traverse(handleDbReadErr)
    } yield connections
  }

  override def removeConnection(gameId: GameId, address: PlayerAddress): F[Unit] = {
    handleDbErr(scanamo.exec(connections.delete("gameId" === gameId.gid and "address" === address.address)))
  }

  private def handleDbReadErr[A](
      result: Either[DynamoReadError, A]
  ): F[A] = {
    Async[F].fromEither {
      result.left.map(dynamoReadFailure)
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
      case failures: Failures => failures
      case NonFatal(err) =>
        Failures(
          "unhandled DynamoDB error",
          "error fetching saved data",
          exception = Some(err)
        )
    }
}

/**
 * Scanamo's transactions drop the conditions on puts, so we build transaction items ourselves.
 */
object DynamoDbDatabase {
  val concurrentChangeFailure: Failure = Failure(
    "Transaction conflicted with a concurrent change",
    "someone else changed the game at the same time, please check and try again.",
  )

  def put[V: DynamoFormat](tableName: String, item: V): TransactWriteItem = {
    TransactWriteItem.builder().put(
      Put.builder()
        .tableName(tableName)
        .item(DynamoFormat[V].write(item).asObject.get.toJavaMap)
        .build()
    ).build()
  }

  def conditionalPut[V: DynamoFormat, C: ConditionExpression](tableName: String, item: V, condition: C): TransactWriteItem = {
    val requestCondition = ConditionExpression[C].apply(condition).runEmptyA.value
    TransactWriteItem.builder().put(
      Put.builder()
        .tableName(tableName)
        .item(DynamoFormat[V].write(item).asObject.get.toJavaMap)
        .conditionExpression(requestCondition.expression)
        .expressionAttributeNames(requestCondition.attributes.names.asJava)
        .expressionAttributeValues(requestCondition.attributes.values.toExpressionAttributeValues.orNull)
        .build()
    ).build()
  }

  def conditionCheck[C: ConditionExpression](tableName: String, key: UniqueKey[?], condition: C): TransactWriteItem = {
    val requestCondition = ConditionExpression[C].apply(condition).runEmptyA.value
    TransactWriteItem.builder().conditionCheck(
      ConditionCheck.builder()
        .tableName(tableName)
        .key(key.toDynamoObject.toJavaMap)
        .conditionExpression(requestCondition.expression)
        .expressionAttributeNames(requestCondition.attributes.names.asJava)
        .expressionAttributeValues(requestCondition.attributes.values.toExpressionAttributeValues.orNull)
        .build()
    ).build()
  }

  /**
   * Each write is paired with the failure to report if its condition isn't met.
   *
   * Transactions that touch the same item at the same moment conflict, even if both only check it (e.g. concurrent joins).
   * Retrying is safe because the conditions are checked again.
   */
  def runTransaction[F[_]: Async](client: DynamoDbAsyncClient, writes: (TransactWriteItem, Failure)*): F[Unit] = {
    val request = TransactWriteItemsRequest.builder().transactItems(writes.map(_._1).asJava).build()
    def attempt(retries: Int): F[Unit] =
      Async[F].fromCompletableFuture(Async[F].delay(client.transactWriteItems(request))).void
        .recoverWith {
          case e: TransactionCanceledException if retries > 0 && isConflictOnly(e) =>
            Async[F].sleep(conflictRetryDelay) >> attempt(retries - 1)
        }
    attempt(conflictRetries).adaptError {
      case e: TransactionCanceledException =>
        transactionFailure(e, writes.map(_._2))
    }
  }

  private val conflictRetries = 2
  private val conflictRetryDelay = 50.millis

  def isConflictOnly(e: TransactionCanceledException): Boolean = {
    val codes = e.cancellationReasons.asScala.flatMap(reason => Option(reason.code)).toSet
    codes.contains("TransactionConflict") && !codes.contains("ConditionalCheckFailed")
  }

  /**
   * Cancellation reasons line up with the transaction's items.
   * Unrecognised cancellations are returned unchanged.
   */
  def transactionFailure(e: TransactionCanceledException, conditionFailures: Seq[Failure]): Throwable = {
    val codes = e.cancellationReasons.asScala.map(reason => Option(reason.code)).toList
    codes.zip(conditionFailures).collectFirst { case (Some("ConditionalCheckFailed"), failure) => failure }
      .orElse(Option.when(codes.contains(Some("TransactionConflict")))(concurrentChangeFailure))
      .fold[Throwable](e)(_.copy(exception = Some(e)).asFailures)
  }
}
