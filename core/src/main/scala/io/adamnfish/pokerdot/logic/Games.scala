package io.adamnfish.pokerdot.logic

import io.adamnfish.pokerdot.models.{Attempt, Failures, Game, GameDb, GameId, JoinGame, Player, PlayerAddress, PlayerDb, PlayerId, PlayerKey}
import zio.IO


/**
 * Game implementation functionality.
 */
object Games {
  def updatePlayerAddress(player: Player, playerAddress: PlayerAddress): Player = {
    player.copy(
      playerAddress = playerAddress
    )
  }

  def ensurePlayerKey(game: Game, playerId: PlayerId, playerKey: PlayerKey): Attempt[Player] = {
    game.players.find(_.playerId == playerId) match {
      case None =>
        IO.fail(
          Failures(
            "Couldn't validate key for player that does not exist",
            "Couldn't find you in the game",
          )
        )
      case Some(player) if player.playerKey == playerKey =>
        IO.succeed(player)
      case _ =>
        IO.fail {
          Failures(
            "Invalid player key",
            "Couldn't authenticate you for this game",
          )
        }
    }
  }

  def requireGame(gameDbOpt: Option[GameDb], gid: String): Attempt[GameDb] = {
    gameDbOpt match {
      case Some(gameDb) =>
        IO.succeed(gameDb)
      case None =>
        IO.fail {
          Failures(
            s"Game not found for lookup $gid",
            "Couldn't find game. If it's old it may have been automatically deleted?",
          )
        }
    }
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

  def addPlayerIds(gameDb: GameDb, playerDbs: List[PlayerDb]): GameDb = {
    gameDb.copy(
      playerIds = playerDbs.map(_.playerId)
    )
  }

  def addPlayer(game: Game, player: Player): Game = {
    game.copy(
      players = player :: game.players
    )
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
}
