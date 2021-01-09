package io.adamnfish.pokerdot.logic

import io.adamnfish.pokerdot.logic.Play.dealHoles
import io.adamnfish.pokerdot.models._
import io.adamnfish.pokerdot.services.{Dates, Rng}

import java.util.UUID


/**
 * Game implementation functionality.
 */
object Games {
  def newGame(gameName: String, trackStacks: Boolean, dates: Dates, initialState: Long): Game = {
    val round = Play.generateRound(PreFlop, initialState)
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
      isHost = isHost
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

  def start(game: Game, now: Long, timerLevels: List[TimerLevel], startingStacks: Option[Int]): Game = {
    val deck = Play.deckOrder(game.seed)
    val dealtPlayers = dealHoles(game.players, deck)
    game.copy(
      players = dealtPlayers,
      started = true,
      startTime = now,
      trackStacks = startingStacks.isDefined,
      button = 0,
      timer =
        if (timerLevels.isEmpty) {
          None
        } else {
          Some(TimerStatus(now, None, timerLevels))
        },
    )
  }

  def advancePhase(game: Game, rng: Rng): Either[Failures, (Game, Set[PlayerId], Option[(List[PlayerWinnings], List[PotWinnings])])] = {
    val betAmount = game.players.map(_.bet).max
    val playersYetToAct = game.players.filter(Play.playerIsYetToAct(betAmount))

    if (playersYetToAct.nonEmpty) {
      val message =
        playersYetToAct match {
          case lastPlayer :: Nil =>
            s"${lastPlayer.screenName} needs to act before the round is finished"
          case _ =>
            s"${playersYetToAct.size} players still need to act"
        }
      Left(
        Failures(
          s"Cannot advance phase while ${playersYetToAct.length} players have not yet acted",
          message,
        )
      )
    } else {
      game.round.phase match {
        case PreFlop =>
          val updatedPlayers = game.players.map(resetPlayerForNextPhase)
          Right(
            game.copy(
              round = game.round.copy(phase = Flop),
              players = updatedPlayers,
            ),
            filteredPlayerIds(updatedPlayers) { player =>
              !player.busted && !player.folded
            },
            None,
          )
        case Flop =>
          val updatedPlayers = game.players.map(resetPlayerForNextPhase)
          Right(
            game.copy(
              round = game.round.copy(phase = Turn),
              players = updatedPlayers,
            ),
            filteredPlayerIds(updatedPlayers) { player =>
              !player.busted && !player.folded
            },
            None,
          )
        case Turn =>
          val updatedPlayers = game.players.map(resetPlayerForNextPhase)
          Right(
            game.copy(
              round = game.round.copy(phase = River),
              players = updatedPlayers,
            ),
            filteredPlayerIds(updatedPlayers) { player =>
              !player.busted && !player.folded
            },
            None,
          )
        case River =>
          val playerHands = PokerHands.bestHands(game.round, game.players)
          val potsWinnings = PokerHands.winnings(playerHands)
          val playersWinnings = PokerHands.playerWinnings(potsWinnings, game.button,
            playerOrder = game.players.map(_.playerId),
            playerHands = playerHands.map(ph => ph.player.playerId -> ph.hand),
          )
          val updatedPlayers = game.players.map(resetPlayerForShowdown(playersWinnings))
          Right(
            game.copy(
              round = game.round.copy(phase = Showdown),
              players = updatedPlayers,
            ),
            filteredPlayerIds(updatedPlayers) { player =>
              !player.busted && !player.folded
            },
            Some((playersWinnings, potsWinnings)),
          )
        case Showdown =>
          val nextState = rng.nextState(game.seed)
          val nextDeck = Play.deckOrder(nextState)
          // finalise player payments, bust players if required, shuffle, deal new cards, set up new round
          Right(
            game.copy(
              round = game.round.copy(phase = PreFlop),
              players = dealHoles(game.players.map(resetPlayerForNextRound), nextDeck),
              seed = nextState
            ),
            // between rounds we'll update everyone that's still in the game to deal cards,
            // including players that have folded
            filteredPlayerIds(game.players) { player =>
              !player.busted
            },
            None,
          )
      }
    }
  }

  private def filteredPlayerIds(players: List[Player])(pred: Player => Boolean): Set[PlayerId] = {
    players.filter(pred).map(_.playerId).toSet
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
        "The game has already started",
      )
    }
    else Right(())
  }

  def ensureStarted(game: Game): Either[Failures, Unit] = {
    if (game.started) Right(())
    else Left {
      Failures(
        "game has not started",
        "The game has not started",
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

  def ensurePlayerCount(players: List[Player]): Either[Failures, Unit] = {
    if (players.size > 20) {
      Left {
        Failures(
          "Max player count exceeded",
          "There are already 20 players in this game, which is the maximum number",
        )
      }
    } else {
      Right(())
    }
  }

  def ensureNotAlreadyPlaying(players: List[Player], playerAddress: PlayerAddress): Either[Failures, Unit] = {
    if (players.exists(_.playerAddress == playerAddress))
      Left {
        Failures(
          "Duplicate player address, joining game failed",
          "You can't join the same game twice",
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
            "Couldn't find you in the game",
          )
        }
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

  def ensureHost(players: List[Player], playerKey: PlayerKey): Either[Failures, Player] = {
    players.find(_.playerKey == playerKey) match {
      case None =>
        Left {
          Failures(
            "Couldn't validate host key for player that does not exist",
            "Couldn't find you in the game",
          )
        }
      case Some(player) if player.isHost =>
        Right(player)
      case _ =>
        Left {
          Failures(
            "Invalid player key, not the host",
            "You are not the game's host"
          )
        }
    }
  }
}
