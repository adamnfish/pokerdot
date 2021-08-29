package io.adamnfish.pokerdot.logic

import io.adamnfish.pokerdot.logic.Utils.RichList
import io.adamnfish.pokerdot.models.{ActionSummary, BigBlind, Failures, Flop, FlopSummary, Game, GameDb, GameId, GameStatus, GameSummary, NoBlind, Player, PlayerAddress, PlayerDb, PlayerId, PlayerKey, PlayerSummary, PlayerWinnings, PotWinnings, PreFlop, PreFlopSummary, River, RiverSummary, Round, RoundSummary, RoundWinnings, SelfSummary, Showdown, ShowdownSummary, SmallBlind, Spectator, SpectatorSummary, Turn, TurnSummary}


object Representations {
  def gameToDb(game: Game): GameDb = {
    GameDb(
      gameCode = game.gameCode,
      gameId = game.gameId.gid,
      expiry = game.expiry,
      gameName = game.gameName,
      playerIds = game.players.map(_.playerId.pid),
      spectatorIds = game.spectators.map(_.playerId.pid),
      seed = game.seed,
      phase = game.round.phase,
      smallBlind = game.round.smallBlind,
      inTurn = game.inTurn.map(_.pid),
      button = game.button,
      started = game.started,
      startTime = game.startTime,
      trackStacks = game.trackStacks,
      timer = game.timer
    )
  }

  def playerToDb(player: Player): PlayerDb = {
    PlayerDb(
      gameId = player.gameId.gid,
      playerId = player.playerId.pid,
      expiry = player.expiry,
      playerAddress = player.playerAddress.address,
      playerKey = player.playerKey.key,
      screenName = player.screenName,
      stack = player.stack,
      pot = player.pot,
      bet = player.bet,
      checked = player.checked,
      folded = player.folded,
      busted = player.busted,
      hole = player.hole,
      holeVisible = player.holeVisible,
      isHost = player.isHost,
      isAdmin = player.isAdmin,
      blind = player.blind match {
        case NoBlind => 0
        case SmallBlind => 1
        case BigBlind => 2
      },
      isSpectator = false,
    )
  }

  def spectatorToDb(spectator: Spectator): PlayerDb = {
    PlayerDb(
      gameId = spectator.gameId.gid,
      playerId = spectator.playerId.pid,
      expiry = spectator.expiry,
      playerAddress = spectator.playerAddress.address,
      playerKey = spectator.playerKey.key,
      screenName = spectator.screenName,
      stack = 0,
      pot = 0,
      bet = 0,
      checked = false,
      folded = false,
      busted = false,
      hole = None,
      holeVisible = false,
      isHost = spectator.isHost,
      isAdmin = spectator.isAdmin,
      blind = 0,
      isSpectator = true,
    )
  }

  def allPlayerDbs(players: List[Player]): List[PlayerDb] = {
    players.map(playerToDb)
  }

  def activePlayerDbs(players: List[Player]): List[PlayerDb] = {
    players.map(playerToDb).filterNot { pdb =>
      pdb.folded || pdb.busted
    }
  }

  def filteredPlayerDbs(players: List[Player], allowlist: Set[PlayerId]): Either[Failures, List[PlayerDb]] = {
    val filtered = allPlayerDbs(players.filter(p => allowlist.contains(p.playerId)))
    if (filtered.isEmpty) {
      Left {
        Failures(
          "Trying to get playerDb for player ID that does not exist",
          "there was a problem trying to save a user that could not be found.",
        )
      }
    } else {
      Right(filtered)
    }
  }

  def gameFromDb(gameDb: GameDb, playerDbs: List[PlayerDb]): Either[Failures, Game] = {
    for {
      // checks we have a player db for each player / spectator ID in the game
      playerDbs <- gameDb.playerIds.eTraverse(lookupPlayerDb(gameDb.gameId, playerDbs))
      spectatorDbs <- gameDb.spectatorIds.eTraverse(lookupPlayerDb(gameDb.gameId, playerDbs))
      // make sure the current player exists
      inTurn <- gameDb.inTurn
        .fold[Either[Failures, Option[PlayerDb]]](Right(None)) { playerId =>
          lookupPlayerDb(gameDb.gameId, playerDbs)(playerId).map(Some(_))
        }
      round = Play.generateRound(gameDb.phase, gameDb.smallBlind, gameDb.seed)
    } yield {
      Game(
        gameId = GameId(gameDb.gameId),
        gameCode = gameDb.gameCode,
        gameName = gameDb.gameName,
        players = playerDbs.map(playerFromDb),
        spectators = spectatorDbs.map(spectatorFromDb),
        seed = gameDb.seed,
        round = round,
        inTurn = inTurn.map(pdb => PlayerId(pdb.playerId)),
        button = gameDb.button,
        started = gameDb.started,
        startTime = gameDb.startTime,
        expiry = gameDb.expiry,
        trackStacks = gameDb.trackStacks,
        timer = gameDb.timer
      )
    }
  }

  def playerFromDb(playerDb: PlayerDb): Player = {
    Player(
      gameId = GameId(playerDb.gameId),
      playerId = PlayerId(playerDb.playerId),
      expiry = playerDb.expiry,
      playerAddress = PlayerAddress(playerDb.playerAddress),
      playerKey = PlayerKey(playerDb.playerKey),
      screenName = playerDb.screenName,
      stack = playerDb.stack,
      pot = playerDb.pot,
      bet = playerDb.bet,
      checked = playerDb.checked,
      folded = playerDb.folded,
      busted = playerDb.busted,
      hole = playerDb.hole,
      isHost = playerDb.isHost,
      isAdmin = playerDb.isAdmin,
      blind = playerDb.blind match {
        case 1 => SmallBlind
        case 2 => BigBlind
        case _ => NoBlind
      },
      holeVisible = playerDb.holeVisible,
    )
  }

  def spectatorFromDb(playerDb: PlayerDb): Spectator = {
    Spectator(
      gameId = GameId(playerDb.gameId),
      playerId = PlayerId(playerDb.playerId),
      expiry = playerDb.expiry,
      playerAddress = PlayerAddress(playerDb.playerAddress),
      playerKey = PlayerKey(playerDb.playerKey),
      screenName = playerDb.screenName,
      isHost = playerDb.isHost,
      isAdmin = playerDb.isAdmin,
    )
  }

  private def lookupPlayerDb(gameId: String, playerDbs: List[PlayerDb])(playerId: String): Either[Failures, PlayerDb] = {
    playerDbs
      .find(_.playerId == playerId)
      .toRight {
        Failures(
          s"Player $playerId not found in database for game $gameId",
          s"could not load all players",
        )
      }
  }

  def gameStatus(game: Game, player: Player, actionSummary: ActionSummary): GameStatus = {
    GameStatus(
      self = summariseSelf(player),
      game = summariseGame(game),
      action = actionSummary,
    )
  }

  def gameStatus(game: Game, spectator: Spectator, actionSummary: ActionSummary): GameStatus = {
    GameStatus(
      self = summariseSelf(spectator),
      game = summariseGame(game),
      action = actionSummary,
    )
  }

  def roundWinnings(game: Game, player: Player, potWinnings: List[PotWinnings], playerWinnings: List[PlayerWinnings]): RoundWinnings = {
    RoundWinnings(
      self = summariseSelf(player),
      game = summariseGame(game),
      pots = potWinnings,
      players = playerWinnings
    )
  }

  def summariseRound(round: Round, players: List[Player]): RoundSummary = {
    round.phase match {
      case PreFlop =>
        PreFlopSummary()
      case Flop =>
        FlopSummary(
          flop1 = round.flop1,
          flop2 = round.flop2,
          flop3 = round.flop3,
        )
      case Turn =>
        TurnSummary(
          flop1 = round.flop1,
          flop2 = round.flop2,
          flop3 = round.flop3,
          turn = round.turn,
        )
      case River =>
        RiverSummary(
          flop1 = round.flop1,
          flop2 = round.flop2,
          flop3 = round.flop3,
          turn = round.turn,
          river = round.river,
        )
      case Showdown =>
        ShowdownSummary(
          flop1 = round.flop1,
          flop2 = round.flop2,
          flop3 = round.flop3,
          turn = round.turn,
          river = round.river,
          holes = Play.lookupHoles(players)
        )
    }
  }

  def summariseGame(game: Game): GameSummary = {
    GameSummary(
      gameId = game.gameId,
      gameCode = game.gameCode,
      gameName = game.gameName,
      players = game.players.map(summarisePlayer),
      spectators = game.spectators.map(summariseSpectator),
      round = summariseRound(game.round, game.players),
      smallBlind = game.round.smallBlind,
      inTurn = game.inTurn,
      button = game.button,
      started = game.started,
      startTime = game.startTime,
      trackStacks = game.trackStacks,
      timer = game.timer,
    )
  }

  def summariseSelf(player: Player): SelfSummary = {
    SelfSummary(
      playerId = player.playerId,
      screenName = player.screenName,
      stack = player.stack,
      pot = player.pot,
      bet = player.bet,
      folded = player.folded,
      busted = player.busted,
      hole = player.hole,
      isHost = player.isHost,
      isAdmin = player.isAdmin,
    )
  }

  def summariseSelf(spectator: Spectator): SpectatorSummary = {
    SpectatorSummary(
      playerId = spectator.playerId,
      screenName = spectator.screenName,
      isHost = spectator.isHost,
      isAdmin = spectator.isAdmin,
    )
  }

  def summarisePlayer(player: Player): PlayerSummary = {
    PlayerSummary(
      playerId = player.playerId,
      screenName = player.screenName,
      stack = player.stack,
      pot = player.pot,
      bet = player.bet,
      folded = player.folded,
      busted = player.busted,
      isHost = player.isHost,
      isAdmin = player.isAdmin,
      hole =
        if (player.holeVisible) player.hole
        else None
    )
  }

  def summariseSpectator(spectator: Spectator): SpectatorSummary = {
    SpectatorSummary(
      playerId = spectator.playerId,
      screenName = spectator.screenName,
      isHost = spectator.isHost,
      isAdmin = spectator.isAdmin,
    )
  }
}
