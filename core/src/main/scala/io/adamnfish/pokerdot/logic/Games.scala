package io.adamnfish.pokerdot.logic

import java.util.UUID
import io.adamnfish.pokerdot.logic.Play.{dealHoles, generateRound}
import io.adamnfish.pokerdot.models._
import io.adamnfish.pokerdot.services.Dates
import io.adamnfish.pokerdot.utils.Rng
import io.adamnfish.pokerdot.utils.Rng.Seed


/**
 * Game implementation functionality.
 */
object Games {
  def newGame(gameName: String, trackStacks: Boolean, dates: Dates): Seed[Game] = {
    for {
      gameSeed <- Rng.next
      round <- generateRound(PreFlop)
    } yield {
      Game(
        gameId = GameId(UUID.randomUUID().toString),
        expiry = dates.expires(),
        gameName = gameName,
        players = Nil,
        spectators = Nil,
        seed = gameSeed,
        round = round,
        inTurn = None,
        button = 0,
        started = false,
        startTime = dates.now(),
        trackStacks = trackStacks,
        timer = None,
      )
    }
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

  def start(game: Game, now: Long, timerLevels: List[TimerLevel], startingStacks: Option[Int]): Seed[Game] = {
    dealHoles(game.players).map { players =>
      game.copy(
        players = players,
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
  }

  def advancePhase(game: Game): Either[Failures, (Game, List[Player], List[PotWinnings])] = {
    // ensure the round is finished
    // all players are passed, folded, busted

    val activePlayers = game.players.filter { player =>
      !player.busted && !player.folded
    }
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
          Right(
            game.copy(
              round = game.round.copy(phase = Flop),
              players = game.players.map(resetPlayerForNextPhase),
            ),
            activePlayers,
            Nil,
          )
        case Flop =>
          Right(
            game.copy(
              round = game.round.copy(phase = Turn),
              players = game.players.map(resetPlayerForNextPhase),
            ),
            activePlayers,
            Nil,
          )
        case Turn =>
          Right(
            game.copy(
              round = game.round.copy(phase = River),
              players = game.players.map(resetPlayerForNextPhase),
            ),
            activePlayers,
            Nil,
          )
        case River =>
          val potsWinnings = PokerHands.potWinnings(game.round, game.players)
          val playersWinnings = PokerHands.playerWinnings(potsWinnings, game.button, game.players.map(_.playerId))
          Right(
            game.copy(
              round = game.round.copy(phase = Showdown),
              players = game.players.map(resetPlayerForShowdown(playersWinnings)),
            ),
            activePlayers,
            potsWinnings
          )
        case Showdown =>
          Right(
            game.copy(
              round = game.round.copy(phase = PreFlop),
              players = game.players.map(resetPlayerForNextRound),
            ),
            // between rounds we'll update everyone that's still in the game
            // including players that have folded
            game.players.filter { player =>
              !player.busted
            },
            Nil,
          )
      }
    }
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
  def resetPlayerForShowdown(playersWinnings: Map[PlayerId, Int])(player: Player): Player = {
    resetPlayerForNextPhase(player).copy(
      stack = player.stack + playersWinnings.getOrElse(player.playerId, 0)
    )
  }

  /**
   * As with next phase, this resets the phase state.
   * We're also done with the pots at this point having already updated player stacks before the showdown.
   * Additionally, with a new round starting we can reset the fold status of all players.
   */
  def resetPlayerForNextRound(player: Player): Player = {
    resetPlayerForNextPhase(player).copy(
      pot = 0,
      folded = false,
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
