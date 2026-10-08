package io.adamnfish.pokerdot.persistence

import io.adamnfish.pokerdot.models.{Failure, Failures}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import software.amazon.awssdk.services.dynamodb.model.{CancellationReason, TransactionCanceledException}


class DynamoDbDatabaseTest extends AnyFreeSpec with Matchers {
  "transactionFailure" - {
    val gameFailure = Failure("game condition failed", "game changed")
    val playerFailure = Failure("player condition failed", "player missing")

    def cancelled(codes: String*): TransactionCanceledException =
      TransactionCanceledException.builder()
        .cancellationReasons(codes.map(code => CancellationReason.builder().code(code).build())*)
        .build()

    "returns the failure paired with the item whose condition failed" in {
      DynamoDbDatabase.transactionFailure(cancelled("None", "ConditionalCheckFailed"), List(gameFailure, playerFailure)) match {
        case failures: Failures => failures.failures.map(_.logMessage) shouldEqual List(playerFailure.logMessage)
        case other => fail(s"expected Failures, got $other")
      }
    }

    "attaches the exception" in {
      val e = cancelled("ConditionalCheckFailed", "None")
      DynamoDbDatabase.transactionFailure(e, List(gameFailure, playerFailure)) match {
        case failures: Failures => failures.exception shouldEqual Some(e)
        case other => fail(s"expected Failures, got $other")
      }
    }

    "returns the concurrent change failure for a transaction conflict" in {
      DynamoDbDatabase.transactionFailure(cancelled("TransactionConflict", "None"), List(gameFailure, playerFailure)) match {
        case failures: Failures => failures.failures.map(_.userMessage) shouldEqual List(DynamoDbDatabase.concurrentChangeFailure.userMessage)
        case other => fail(s"expected Failures, got $other")
      }
    }

    "prefers a failed condition over a conflict" in {
      DynamoDbDatabase.transactionFailure(cancelled("TransactionConflict", "ConditionalCheckFailed"), List(gameFailure, playerFailure)) match {
        case failures: Failures => failures.failures.map(_.logMessage) shouldEqual List(playerFailure.logMessage)
        case other => fail(s"expected Failures, got $other")
      }
    }

    "returns other cancellations unchanged" in {
      val e = cancelled("ThrottlingError", "None")
      DynamoDbDatabase.transactionFailure(e, List(gameFailure, playerFailure)) shouldEqual e
    }
  }
}
