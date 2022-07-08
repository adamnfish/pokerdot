package io.adamnfish.pokerdot.logic

import io.adamnfish.pokerdot.models._

import scala.annotation.tailrec


object Logs {
  def gameStartEvents(now: Long, game: Game): Either[Failures, List[GameLogEntry]] = {
    newRoundEvents(now + 1, game).map { roundAndPhaseEvents =>
      GameLogEntry(
        game.gameId, now, GameStartEvent(game.players.map(_.playerId))
      ) :: roundAndPhaseEvents
    }
  }

  def newRoundEvents(now: Long, game: Game): Either[Failures, List[GameLogEntry]] = {
    for {
      bbPlayer <- game.players
        .find(_.blind == BigBlind)
        .toRight {
          Failures(
            "Can't write new round event without a BigBlind player",
            "couldn't set up the blinds for the new round",
          )
        }
      sbPlayerId = game.players
        .find(_.blind == SmallBlind)
        .map(_.playerId)
    } yield {
      List(
        GameLogEntry(
          game.gameId, now,
          NewRoundEvent(
            game.seed, game.button, Some(game.round.smallBlind),
            sbPlayerId, bbPlayer.playerId, game.players.map(p => p.stack + p.bet)
          )
        ),
        GameLogEntry(
          game.gameId, now + 1,
          NewPhaseEvent(PreFlop)
        ),
      )
    }
  }

  def newPhaseEvent(now: Long, game: Game): GameLogEntry = {
    GameLogEntry(
      game.gameId, now, NewPhaseEvent(game.round.phase)
    )
  }

  def checkEvent(now: Long, check: Check): GameLogEntry = {
    GameLogEntry(
      check.gameId, now, CheckEvent(check.playerId)
    )
  }

  def betEvent(now: Long, bet: Bet): GameLogEntry = {
    GameLogEntry(
      bet.gameId, now, BetEvent(bet.playerId, bet.betAmount)
    )
  }

  def foldEvent(now: Long, fold: Fold): GameLogEntry = {
    GameLogEntry(
      fold.gameId, now, FoldEvent(fold.playerId)
    )
  }

  def abandonRoundEvents(now: Long, game: Game): Either[Failures, List[GameLogEntry]] = {
    newRoundEvents(now + 1, game).map { nre =>
      GameLogEntry(
        game.gameId, now, AbandonRoundEvent()
      ) :: nre
    }
  }

  def gameEndEvent(now: Long, gameId: GameId, winningPlayer: PlayerId): GameLogEntry = {
    GameLogEntry(
      gameId, now, GameEndEvent(winningPlayer)
    )
  }

  def tryToGetAllPhaseEvents(events: List[GameLogEntryDb]): (List[GameLogEntryDb], Boolean) = {
    @tailrec
    def loop(remainingEvents: List[GameLogEntryDb], accEvents: List[GameLogEntryDb]): (List[GameLogEntryDb], Boolean) = {
      remainingEvents match {
        case Nil =>
          // we ran into the end of the fetched data before finding the end of the phase
          (accEvents, false)
        case (event @ GameLogEntryDb(_, _, NP(_))) :: _ =>
          // New Phase event means we're done searching, return what we have found
          (event :: accEvents, true)
        case (GameLogEntryDb(_, _, GE(_))) :: _ =>
          // Game End event means there's no active phase
          (Nil, true)
        case (event @ GameLogEntryDb(_, _, GS(_))) :: _ =>
          // Game Start event means we're done - this should be an impossible state though
          (accEvents, true)
        case (event @ GameLogEntryDb(_, _, NR(_, _, _, _, _, _))) :: _ =>
          // New Round event means anything after this is a previous phase
          (accEvents, true)
        case (GameLogEntryDb(_, _, AR())) :: _ =>
          // Abandon Round event means anything after this is a previous phase
          (accEvents, true)
        case event :: tail =>
          // a phase event, keep searching
          loop(tail, event :: accEvents)
      }
    }
    // special case no events as being ok, for edge cases
    // (resumed games that pre-date logs, loss of logs)
    if (events.isEmpty) {
      (Nil, true)
    } else {
      val (phaseEvents, finished) = loop(events, Nil)
      (phaseEvents.reverse, finished)
    }
  }
}
