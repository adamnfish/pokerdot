package io.adamnfish.pokerdot.logic

import io.adamnfish.pokerdot.logic.Play.dealHoles
import io.adamnfish.pokerdot.logic.Utils.orderFromList
import io.adamnfish.pokerdot.models._
import io.adamnfish.pokerdot.services.{Database, Clock}
import zio.ZIO

import java.time.{Duration, Instant}
import java.util.UUID


/**
 * Game implementation functionality.
 */
object Games {
  def newGame(gameName: String, trackStacks: Boolean, clock: Clock, initialState: Long): Game = {
    val round = Play.generateRound(PreFlop, 0, initialState)
    val gameId = GameId(UUID.randomUUID().toString)
    Game(
      gameId = gameId,
      gameCode = gameCode(gameId), // try this, we can replace it with a longer unique prefix if required
      expiry = expiryTime(clock.now()),
      gameName = gameName,
      players = Nil,
      spectators = Nil,
      seed = initialState,
      round = round,
      inTurn = None,
      button = 0,
      started = false,
      startTime = clock.now(),
      trackStacks = trackStacks,
      timer = None,
    )
  }

  def newPlayer(gameId: GameId, screenName: String, isHost: Boolean, playerAddress: PlayerAddress, clock: Clock): Player = {
    val playerId = PlayerId(UUID.randomUUID().toString)
    val playerKey = PlayerKey(UUID.randomUUID().toString)
    Player(
      gameId = gameId,
      playerId = playerId,
      expiry = expiryTime(clock.now()),
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

  def newSpectator(gameId: GameId, screenName: String, isHost: Boolean, playerAddress: PlayerAddress, clock: Clock): Spectator = {
    val playerId = PlayerId(UUID.randomUUID().toString)
    val playerKey = PlayerKey(UUID.randomUUID().toString)
    Spectator(
      gameId = gameId,
      playerId = playerId,
      expiry = expiryTime(clock.now()),
      playerAddress = playerAddress,
      playerKey = playerKey,
      screenName = screenName,
      isHost = isHost,
      isAdmin = isHost,
    )
  }

  def updatePlayerAddress(player: Player, playerAddress: PlayerAddress): Option[Player] = {
    if (player.playerAddress != playerAddress) Some {
      player.copy(
        playerAddress = playerAddress
      )
    } else None
  }

  def addPlayerIds(gameDb: GameDb, playerDbs: List[PlayerDb]): GameDb = {
    val playersFromDbs = playerDbs.filterNot(_.isSpectator)
    val spectatorsFromDbs = playerDbs.filter(_.isSpectator)
    val allPlayerIds = (gameDb.playerIds ++ playersFromDbs.map(_.playerId)).distinct
    val allSpectatorIds = (gameDb.spectatorIds ++ spectatorsFromDbs.map(_.playerId)).distinct
    gameDb.copy(
      playerIds = allPlayerIds,
      spectatorIds = allSpectatorIds,
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

  def makeUniquePrefix(gameId: GameId, persistence: Database, fn: (GameId, Int, Database) => Attempt[Boolean]): Attempt[String] = {
    val min = 4
    val max = 10
    def loop(prefixLength: Int): Attempt[String] = {
      fn(gameId, prefixLength, persistence).flatMap {
        case true =>
          ZIO.succeed(gameId.gid.take(prefixLength))
        case false if prefixLength < max =>
          loop(prefixLength + 1)
        case _ =>
          ZIO.fail(
            Failures("Couldn't create unique prefix of GameID", "couldn't set up game with a join code")
          )
      }
    }
    loop(min)
  }

  def start(game: Game, now: Long, initialSmallBlind: Option[Int], timerConfig: Option[List[TimerLevel]], startingStack: Option[Int], playerOrder: List[PlayerId]): Game = {
    val deck = Play.deckOrder(game.seed)
    val smallBlind = initialSmallBlind.orElse {
      timerConfig.collect {
        case RoundLevel(_, roundBlind) :: _ =>
          roundBlind
      }
    }.getOrElse(0)

    val orderedPlayers = orderFromList(game.players, playerOrder)(_.playerId)
    val dealtPlayers = dealHoles(orderedPlayers, deck)
    val dealtPlayersWithInitialStacks = dealtPlayers.zipWithIndex.map { case (p, i) =>
      val (blind, blindAmount) =
        if (dealtPlayers.length == 2) {
          i match {
            case 0 => (SmallBlind, smallBlind)   // dealer is small blind in heads-up
            case 1 => (BigBlind, smallBlind * 2) // dealer's opponent is always BigBlind in heads-up
            case _ => (NoBlind, 0)               // we already checked there are 2 players, but why not
          }
        } else {
          i match {
            case 1 => (SmallBlind, smallBlind)   // left of dealer
            case 2 => (BigBlind, smallBlind * 2) // left of small blind
            case _ => (NoBlind, 0)
          }
        }
      // players can't pay more than they have into the blinds
      p.copy(
        stack = startingStack.fold(0) { initialStackAmount =>
          math.max(0, p.stack + initialStackAmount - blindAmount)
        },
        bet = startingStack.fold(0) { initialStackAmount =>
          math.min(initialStackAmount, blindAmount)
        },
        blind = blind,
      )
    }
    game.copy(
      players = dealtPlayersWithInitialStacks,
      started = true,
      startTime = now,
      trackStacks = startingStack.isDefined,
      button = 0,
      inTurn =
        orderedPlayers match {
          case Nil => None
          case dealer :: Nil =>
            // poker requires at least 2 players
            None
          case dealer :: opponent :: Nil =>
            Some(dealer.playerId)
          case dealer :: smallBlind :: bigBlind :: Nil =>
            Some(dealer.playerId)
          case dealer :: smallBlind :: bigBlind :: nextActive :: _ =>
            Some(nextActive.playerId)
        },
      timer =
        timerConfig.flatMap {
          case Nil =>
            None
          case timerLevels =>
            Some(TimerStatus(now, None, timerLevels))
        },
      round = game.round.copy(smallBlind = smallBlind),
    )
  }

  def updateBlindAction(updateBlind: UpdateBlind): Either[Failures, ActionSummary] = {
    if (updateBlind.timerLevels.isDefined || updateBlind.progress.isDefined) {
      Right(EditTimerSummary())
    } else if (updateBlind.playing.isDefined) {
      Right(TimerStatusSummary(updateBlind.playing.contains(true)))
    } else if (updateBlind.smallBlind.isDefined) {
      Right(EditBlindSummary())
    } else {
      Left(Failures("Couldn't determine action from update blind request", "couldn't update the blinds."))
    }
  }

  def expiryTime(now: Long): Long = {
    Instant
      .ofEpochMilli(now)
      .plus(Duration.ofDays(21))
      .toEpochMilli
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
      // no player interaction is required in the showdown, so mark players as checked
      checked = true,
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
      // the next blind position(s) will be calculated elsewhere - must not be touched here
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
            "couldn't find game, it may have been automatically deleted if it is old?",
          )
        }
    }
  }

  def ensureNotStarted(game: Game): Either[Failures, Unit] = {
    if (game.started) Left {
      Failures(
        "game has already started",
        "the game has already started.",
      )
    }
    else Right(())
  }

  def ensureStarted(game: Game): Either[Failures, Unit] = {
    if (game.started) Right(())
    else Left {
      Failures(
        "game has not started",
        "the game has not started.",
      )
    }
  }

  def ensureNoDuplicateScreenName(game: Game, screenName: String): Either[Failures, Unit] = {
    if (game.players.exists(_.screenName == screenName))
      Left {
        Failures(
          "Duplicate screen name, joining game failed",
          "someone else already has the same name.",
          context = Some("screenName"),
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
          "there are already 20 players in this game, which is the maximum number.",
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
          "a game requires at least 2 players.",
        )
      }
    }
  }

  def ensureNotAlreadyPlaying(players: List[Player], playerAddress: PlayerAddress): Either[Failures, Unit] = {
    if (players.exists(_.playerAddress == playerAddress))
      Left {
        Failures(
          "Duplicate player address, joining game failed",
          "you can't join the same game twice.",
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
            "couldn't find you in the game.",
          )
        }
      case Some(player) if player.playerKey == playerKey =>
        Right(player)
      case _ =>
        Left {
          Failures(
            "Invalid player key",
            "couldn't authenticate you for this game.",
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
            "couldn't find you in the game.",
          )
        }
      case Some(spectator) if spectator.playerKey == playerKey =>
        Right(spectator)
      case _ =>
        Left {
          Failures(
            "Invalid spectator key",
            "couldn't authenticate you for this game.",
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
            "couldn't find you in the game.",
          )
        }
      case Some(player) if player.isHost =>
        Right(player)
      case _ =>
        Left {
          Failures(
            "Invalid player key, not the host",
            "you are not the game's host."
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
            "couldn't find you in the game.",
          )
        }
      case Some(player) if player.isHost =>
        Right(player)
      case _ =>
        Left {
          Failures(
            "Invalid player key, not an admin",
            "you are not a game admin."
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
          "it is not your turn to act.",
        )
      }
    }
  }
}
