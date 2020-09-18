package io.adamnfish.pokerdot.logic

import java.time.ZonedDateTime
import java.util.UUID

import io.adamnfish.pokerdot.logic.Play.generateRound
import io.adamnfish.pokerdot.models._
import io.adamnfish.pokerdot.utils.Rng
import io.adamnfish.pokerdot.utils.Rng.Seed


/**
 * Game implementation functionality.
 */
object Games {
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
      pot = 0,
      bid = 0,
      folded = false,
      busted = false,
      hole = None,
      isCreator = isCreator
    )
  }

  def updatePlayerAddress(player: Player, playerAddress: PlayerAddress): Player = {
    player.copy(
      playerAddress = playerAddress
    )
  }

  def addPlayerIds(gameDb: GameDb, playerDbs: List[PlayerDb]): GameDb = {
    gameDb.copy(
      playerIds = gameDb.playerIds ++ playerDbs.map(_.playerId)
    )
  }

  def addPlayer(game: Game, player: Player): Game = {
    game.copy(
      players = player :: game.players
    )
  }

  def gameCode(gameId: GameId): String = {
    gameId.gid.take(4)
  }

  def normaliseGameCode(joinGame: JoinGame): JoinGame = {
    joinGame.copy(
      gameCode = joinGame.gameCode
        // Zeros look like 'ohs'
        .replace('O', '0')
        .replace('o', '0')
    )
  }

  def requireGame(gameDbOpt: Option[GameDb], gid: String): Either[Failures, GameDb] = {
    gameDbOpt match {
      case Some(gameDb) =>
        Right(gameDb)
      case None =>
        Left {
          Failures(
            s"Game not found for lookup $gid",
            "Couldn't find game. If it's old it may have been automatically deleted?",
          )
        }
    }
  }

  def ensureNotStarted(game: Game): Either[Failures, Unit] = {
    if (game.started) Left {
      Failures(
        "game has already started",
        "The game has already started",
      )
    }
    else Right(())
  }

  def ensureNoDuplicateScreenName(game: Game, screenName: String): Either[Failures, Unit] = {
    if (game.players.exists(_.screenName == screenName))
      Left {
        Failures(
          "Duplicate screen name, joining game failed",
          "Someone else already has the same name!",
        )
      }
    else
      Right(())
  }

  def ensureNotAlreadyPlaying(game: Game, playerAddress: PlayerAddress): Either[Failures, Unit] = {
    if (game.players.exists(_.playerAddress == playerAddress))
      Left {
        Failures(
          "Duplicate player address, joining game failed",
          "You can't join the same game twice",
        )
      }
    else
      Right(())
  }

  def ensurePlayerKey(game: Game, playerId: PlayerId, playerKey: PlayerKey): Either[Failures, Player] = {
    game.players.find(_.playerId == playerId) match {
      case None =>
        Left(
          Failures(
            "Couldn't validate key for player that does not exist",
            "Couldn't find you in the game",
          )
        )
      case Some(player) if player.playerKey == playerKey =>
        Right(player)
      case _ =>
        Left {
          Failures(
            "Invalid player key",
            "Couldn't authenticate you for this game",
          )
        }
    }
  }
}
