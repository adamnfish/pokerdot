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
import org.scanamo.update.{UpdateAndCondition, UpdateExpression}
import software.amazon.awssdk.services.dynamodb.DynamoDbAsyncClient
import software.amazon.awssdk.services.dynamodb.model.{
  ConditionCheck,
  ConditionalCheckFailedException,
  Put,
  TransactWriteItem,
  TransactWriteItemsRequest,
  TransactionCanceledException,
  Update
}

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
      conditionalPut(gameTableName, gameDb, attributeNotExists("gameId")) ->
        Failure(s"Game ${gameDb.gameId} already exists", "couldn't create the game, please try again."),
      conditionalPut(playerTableName, playerDb, attributeNotExists("playerId")) ->
        Failure(s"Player ${playerDb.playerId} already exists", "couldn't create the game, please try again."),
    )
  }

  override def addPlayer(readGameDb: GameDb, playerDb: PlayerDb): F[Unit] = {
    runTransaction(
      conditionCheck(gameTableName, gameKey(readGameDb), "revision" === readGameDb.revision) ->
        Failure(
          s"Game ${readGameDb.gameId} has changed since revision ${readGameDb.revision}, cannot add player",
          "the game changed while you were joining, please try again.",
        ),
      conditionalPut(playerTableName, playerDb, attributeNotExists("playerId")) ->
        Failure(s"Player ${playerDb.playerId} already exists", "couldn't join the game, please try again."),
    )
  }

  override def writeGameAndPlayers(gameDb: GameDb, playerDbs: List[PlayerDb]): F[Unit] = {
    val gameWrite = conditionalPut(
      gameTableName,
      gameDb.copy(revision = gameDb.revision + 1),
      "revision" === gameDb.revision
    ) -> Failure(
      s"Game ${gameDb.gameId} has changed since revision ${gameDb.revision}",
      "someone else changed the game at the same time, please check and try again.",
    )
    val playerWrites = playerDbs.map { playerDb =>
      conditionalUpdate(
        playerTableName,
        "gameId" === playerDb.gameId and "playerId" === playerDb.playerId,
        // only gameplay fields, so we don't overwrite identity fields like the player's address
        set("stack", playerDb.stack) and
          set("pot", playerDb.pot) and
          set("bet", playerDb.bet) and
          set("checked", playerDb.checked) and
          set("folded", playerDb.folded) and
          set("busted", playerDb.busted) and
          set("hole", playerDb.hole) and
          set("holeVisible", playerDb.holeVisible) and
          set("blind", playerDb.blind),
        attributeExists("playerId")
      ) -> Failure(
        s"Player ${playerDb.playerId} does not exist in game ${gameDb.gameId}",
        "there was a problem trying to save a user that could not be found.",
      )
    }
    runTransaction(gameWrite :: playerWrites*)
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

  private def gameKey(gameDb: GameDb): UniqueKey[?] =
    "gameCode" === gameDb.gameCode and "gameId" === gameDb.gameId

  // Scanamo's transactions ignore conditions on puts and updates, so we build these items ourselves

  private def conditionalPut[V: DynamoFormat, C: ConditionExpression](tableName: String, item: V, condition: C): TransactWriteItem = {
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

  private def conditionalUpdate[C: ConditionExpression](tableName: String, key: UniqueKey[?], update: UpdateExpression, condition: C): TransactWriteItem = {
    val requestCondition = ConditionExpression[C].apply(condition).runEmptyA.value
    val attributes = UpdateAndCondition(update, Some(requestCondition)).attributes
    TransactWriteItem.builder().update(
      Update.builder()
        .tableName(tableName)
        .key(key.toDynamoObject.toJavaMap)
        .updateExpression(update.expression)
        .conditionExpression(requestCondition.expression)
        .expressionAttributeNames(attributes.names.asJava)
        .expressionAttributeValues(attributes.values.toExpressionAttributeValues.orNull)
        .build()
    ).build()
  }

  private def conditionCheck[C: ConditionExpression](tableName: String, key: UniqueKey[?], condition: C): TransactWriteItem = {
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

  // each write is paired with the failure to report if its condition is not met
  private def runTransaction(writes: (TransactWriteItem, Failure)*): F[Unit] = {
    val request = TransactWriteItemsRequest.builder().transactItems(writes.map(_._1).asJava).build()
    handleDbErr {
      Async[F].fromCompletableFuture(Async[F].delay(client.transactWriteItems(request))).void
        .adaptError {
          case e: TransactionCanceledException =>
            val failedIndex = e.cancellationReasons.asScala.indexWhere(_.code == "ConditionalCheckFailed")
            if (failedIndex >= 0) writes(failedIndex)._2.copy(exception = Some(e)).asFailures
            else e
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

