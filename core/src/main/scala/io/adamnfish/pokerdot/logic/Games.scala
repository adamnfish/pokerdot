package io.adamnfish.pokerdot.logic

import io.adamnfish.pokerdot.logic.Play.dealHoles
import io.adamnfish.pokerdot.models._
import io.adamnfish.pokerdot.services.Dates

import java.util.UUID


/**
 * Game implementation functionality.
 */
object Games {
  def newGame(gameName: String, trackStacks: Boolean, dates: Dates, initialState: Long): Game = {
    val round = Play.generateRound(PreFlop, 0, initialState)
    Game(
      gameId = GameId(UUID.randomUUID().toString),
      expiry = dates.expires(),
      gameName = gameName,
      players = Nil,
      spectators = Nil,
      seed = initialState,
      round = round,
      inTurn = None,
      button = 0,
      started = false,
      startTime = dates.now(),
      trackStacks = trackStacks,
      timer = None,
    )
  }

  def newPlayer(gameId: GameId, screenName: String, isHost: Boolean, playerAddress: PlayerAddress, dates: Dates): Player = {
    val playerId = PlayerId(UUID.randomUUID().toString)
    val playerKey = PlayerKey(UUID.randomUUID().toString)
    Player(
      gameId = gameId,
      playerId = playerId,
      expiry = dates.expires(),
      screenName = screenName,
      playerAddress = playerAddress,
      playerKey = playerKey,
      stack = 0,
      pot = 0,
      bet = 0,
      checked = false,
      folded = false,
      busted = false,
      hole = None,
      holeVisible = false,
      isHost = isHost,
      isAdmin = isHost,
      blind = NoBlind,
    )
  }

  def newSpectator(gameId: GameId, screenName: String, isHost: Boolean, playerAddress: PlayerAddress, dates: Dates): Spectator = {
    val playerId = PlayerId(UUID.randomUUID().toString)
    val playerKey = PlayerKey(UUID.randomUUID().toString)
    Spectator(
      gameId = gameId,
      playerId = playerId,
      expiry = dates.expires(),
      playerAddress = playerAddress,
      playerKey = playerKey,
      screenName = screenName,
      isHost = isHost,
      isAdmin = isHost,
    )
  }

  def updatePlayerAddress(player: Player, playerAddress: PlayerAddress): Player = {
    player.copy(
      playerAddress = playerAddress
    )
  }

  def addPlayerIds(gameDb: GameDb, playerDbs: List[PlayerDb]): GameDb = {
    val playersFromDbs = playerDbs.filterNot(_.isSpectator)
    val spectatorsFromDbs = playerDbs.filter(_.isSpectator)
    gameDb.copy(
      playerIds = gameDb.playerIds ++ playersFromDbs.map(_.playerId),
      spectatorIds = gameDb.spectatorIds ++ spectatorsFromDbs.map(_.playerId)
    )
  }

  def addPlayer(game: Game, player: Player): Game = {
    game.copy(
      players = player :: game.players
    )
  }

  def addSpectator(game: Game, spectator: Spectator): Game = {
    game.copy(
      spectators = spectator :: game.spectators
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

  def start(game: Game, now: Long, timerLevelsOpt: Option[List[TimerLevel]], startingStacks: Option[Int]): Game = {
    val deck = Play.deckOrder(game.seed)
    val dealtPlayers = dealHoles(game.players, deck)
    val dealtPlayersWithInitialStacks = dealtPlayers.zipWithIndex.map { case (p, i) =>
      val (blind, blindAmount) = i match {
        case 1 => (SmallBlind, game.round.smallBlind)   // left of dealer
        case 2 => (BigBlind, game.round.smallBlind * 2) // left of small blind
        case _ => (NoBlind, 0)
      }
      p.copy(
        stack = startingStacks.fold(0) { initialStackAmount =>
          p.stack + initialStackAmount - blindAmount
        },
        bet = blindAmount,
        blind = blind,
      )
    }
    game.copy(
      players = dealtPlayersWithInitialStacks,
      started = true,
      startTime = now,
      trackStacks = startingStacks.isDefined,
      button = 0,
      timer =
        timerLevelsOpt.flatMap {
          case Nil =>
            None
          case timerLevels =>
            Some(TimerStatus(now, None, timerLevels))
        },
    )
  }

  /**
   * Copies the round's bet over to the player's pot contribution and resets the checked state.
   *
   */
  def resetPlayerForNextPhase(player: Player): Player = {
    player.copy(
      checked = false,
      bet = 0,
      pot = player.pot + player.bet,
    )
  }

  /**
   * In the showdown we've updated player stacks, but left their pots intact.
   * This allows the UI to better show the before / after states for the showdown.
   */
  def resetPlayerForShowdown(playersWinnings: List[PlayerWinnings])(player: Player): Player = {
    resetPlayerForNextPhase(player).copy(
      checked = false,
      stack = player.stack + playersWinnings.find(_.playerId == player.playerId).map(_.winnings).getOrElse(0)
    )
  }

  /**
   * As with next phase, this resets the phase state.
   * We're also done with the pots at this point having already updated player stacks before the showdown.
   * Additionally, with a new round starting we can reset the fold status of all players and bust any players
   * that have run out of money.
   */
  def resetPlayerForNextRound(player: Player): Player = {
    val resetPlayer = resetPlayerForNextPhase(player).copy(
      pot = 0,
      folded = false,
      checked = false,
      blind = NoBlind, // the next blind position(s) will be calculated elsewhere
    )
    if (resetPlayer.stack <= 0) {
      resetPlayer.copy(busted = true)
    } else resetPlayer
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
        "The game has already started.",
      )
    }
    else Right(())
  }

  def ensureStarted(game: Game): Either[Failures, Unit] = {
    if (game.started) Right(())
    else Left {
      Failures(
        "game has not started",
        "The game has not started.",
      )
    }
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

  def ensurePlayerCount(n: Int): Either[Failures, Unit] = {
    if (n >= 20) {
      Left {
        Failures(
          "Max player count exceeded",
          "There are already 20 players in this game, which is the maximum number.",
        )
      }
    } else {
      Right(())
    }
  }

  def ensureStartingPlayerCount(n: Int): Either[Failures, Unit] = {
    if (n > 1) {
      Right(())
    } else {
      Left {
        Failures(
          "Cannot start with one player",
          "A game requires at least 2 players.",
        )
      }
    }
  }

  def ensureNotAlreadyPlaying(players: List[Player], playerAddress: PlayerAddress): Either[Failures, Unit] = {
    if (players.exists(_.playerAddress == playerAddress))
      Left {
        Failures(
          "Duplicate player address, joining game failed",
          "You can't join the same game twice.",
        )
      }
    else
      Right(())
  }

  def ensurePlayerKey(players: List[Player], playerId: PlayerId, playerKey: PlayerKey): Either[Failures, Player] = {
    players.find(_.playerId == playerId) match {
      case None =>
        Left {
          Failures(
            "Couldn't validate key for player that does not exist",
            "Couldn't find you in the game.",
          )
        }
      case Some(player) if player.playerKey == playerKey =>
        Right(player)
      case _ =>
        Left {
          Failures(
            "Invalid player key",
            "Couldn't authenticate you for this game.",
          )
        }
    }
  }

  def ensureSpectatorKey(spectators: List[Spectator], playerId: PlayerId, playerKey: PlayerKey): Either[Failures, Spectator] = {
    spectators.find(_.playerId == playerId) match {
      case None =>
        Left {
          Failures(
            "Couldn't validate key for spectator that does not exist",
            "Couldn't find you in the game.",
          )
        }
      case Some(spectator) if spectator.playerKey == playerKey =>
        Right(spectator)
      case _ =>
        Left {
          Failures(
            "Invalid spectator key",
            "Couldn't authenticate you for this game.",
          )
        }
    }
  }

  def ensureHost(players: List[Player], playerKey: PlayerKey): Either[Failures, Player] = {
    players.find(_.playerKey == playerKey) match {
      case None =>
        Left {
          Failures(
            "Couldn't validate host key for player that does not exist",
            "Couldn't find you in the game.",
          )
        }
      case Some(player) if player.isHost =>
        Right(player)
      case _ =>
        Left {
          Failures(
            "Invalid player key, not the host",
            "You are not the game's host."
          )
        }
    }
  }

  def ensureAdmin(players: List[Player], playerKey: PlayerKey): Either[Failures, Player] = {
    players.find(_.playerKey == playerKey) match {
      case None =>
        Left {
          Failures(
            "Couldn't validate host key for player that does not exist",
            "Couldn't find you in the game.",
          )
        }
      case Some(player) if player.isHost =>
        Right(player)
      case _ =>
        Left {
          Failures(
            "Invalid player key, not an admin",
            "You are not a game admin."
          )
        }
    }
  }

  def ensureActive(inTurn: Option[PlayerId], playerId: PlayerId): Either[Failures, Unit] = {
    if (inTurn.contains(playerId)) {
      Right(())
    } else {
      Left {
        Failures(
          "Active player check failed",
          "It is not your turn to act.",
        )
      }
    }
  }
}
