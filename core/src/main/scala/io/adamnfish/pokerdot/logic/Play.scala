package io.adamnfish.pokerdot.logic

import java.time.ZonedDateTime
import java.util.UUID

import io.adamnfish.pokerdot.models._
import io.adamnfish.pokerdot.utils.Rng
import io.adamnfish.pokerdot.utils.Rng.Seed


object Play {
  def newGame(gameName: String, trackStacks: Boolean): Seed[Game] = {
    val now = ZonedDateTime.now()
    for {
      gameSeed <- Rng.next
      round <- generateRound(PreFlop)
    } yield {
      Game(
        gameId = GameId(UUID.randomUUID().toString),
        gameName = gameName,
        players = Nil,
        spectators = Nil,
        seed = gameSeed,
        round = round,
        inTurn = None,
        button = 0,
        started = false,
        startTime = now,
        expiry = now.plusDays(21).toEpochSecond,
        trackStacks = trackStacks,
        timer = None,
      )
    }
  }

  def newPlayer(gameId: GameId, screenName: String, isCreator: Boolean, playerAddress: PlayerAddress): Player = {
    val playerId = PlayerId(UUID.randomUUID().toString)
    val playerKey = PlayerKey(UUID.randomUUID().toString)
    Player(
      gameId = gameId,
      screenName = screenName,
      playerId = playerId,
      playerAddress = playerAddress,
      playerKey = playerKey,
      stack = 0,
      pot = 0, bid = 0,
      folded = false,
      busted = false,
      hole = None,
      isCreator = isCreator
    )
  }

  def generateRound(phase: Phase): Seed[Round] = {
    for {
      deck <- Rng.shuffledDeck()
    } yield {
      deck match {
        case burn1 :: flop1 :: flop2 :: flop3 :: burn2 :: turn :: burn3 :: river :: _ =>
          Round(
            phase,
            burn1 = burn1,
            flop1 = flop1,
            flop2 = flop2,
            flop3 = flop3,
            burn2 = burn2,
            turn = turn,
            burn3 = burn3,
            river = river,
          )
        case _ =>
          // unreachable code, asking for 8 cards from a full deck will succeed
          throw new RuntimeException(s"Unreachable code: failed to draw cards from shuffled deck `$deck`")
      }
    }
  }

  def hands(players: List[Player]): List[(PlayerId, Hole)] = {
    for {
      activePlayer <- players
        .filterNot(_.busted)
        .filterNot(_.folded)
      privateCards <- activePlayer.hole
    } yield (activePlayer.playerId, privateCards)
  }
}
