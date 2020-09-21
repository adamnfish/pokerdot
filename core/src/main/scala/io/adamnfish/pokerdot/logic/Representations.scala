package io.adamnfish.pokerdot.logic

import io.adamnfish.pokerdot.logic.Utils.RichList
import io.adamnfish.pokerdot.models.{ActionSummary, Failure, Failures, Flop, FlopSummary, Game, GameDb, GameId, GameStatus, GameSummary, Player, PlayerAddress, PlayerDb, PlayerId, PlayerKey, PlayerSummary, PreFlop, PreFlopSummary, Result, ResultSummary, River, RiverSummary, Round, RoundSummary, RoundWinnings, SelfSummary, Showdown, ShowdownSummary, Spectator, SpectatorDb, SpectatorSummary, Turn, TurnSummary}


object Representations {
  def gameToDb(game: Game): GameDb = {
    GameDb(
      gameCode = Games.gameCode(game.gameId),
      gameId = game.gameId.gid,
      gameName = game.gameName,
      playerIds = game.players.map(_.playerId.pid),
      spectatorIds = game.spectators.map(_.playerId.pid),
      seed = game.seed,
      phase = game.round.phase,
      inTurn = game.inTurn.map(_.playerId.pid),
      button = game.button,
      started = game.started,
      startTime = game.startTime,
      expiry = game.expiry,
      trackStacks = game.trackStacks,
      timer = game.timer
    )
  }

  def playerToDb(player: Player): PlayerDb = {
    PlayerDb(
      gameId = player.gameId.gid,
      playerId = player.playerId.pid,
      playerAddress = player.playerAddress.address,
      playerKey = player.playerKey.key,
      screenName = player.screenName,
      stack = player.stack,
      pot = player.pot,
      bid = player.bid,
      folded = player.folded,
      busted = player.busted,
      hole = player.hole,
      isCreator = player.isCreator,
    )
  }

  def spectatorToDb(spectator: Spectator): SpectatorDb = {
    SpectatorDb(
      gameId = spectator.gameId.gid,
      playerId = spectator.playerId.pid,
      playerAddress = spectator.playerAddress.address,
      playerKey = spectator.playerKey.key,
    )
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
      round = Play.generateRound(gameDb.phase).value(gameDb.seed)
    } yield {
      Game(
        gameId = GameId(gameDb.gameId),
        gameName = gameDb.gameName,
        players = playerDbs.map(playerFromDb),
        spectators = spectatorDbs.map(spectatorFromDb),
        seed = gameDb.seed,
        round = round,
        inTurn = inTurn.map(playerFromDb),
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
      playerAddress = PlayerAddress(playerDb.playerAddress),
      playerKey = PlayerKey(playerDb.playerKey),
      screenName = playerDb.screenName,
      stack = playerDb.stack,
      pot = playerDb.pot,
      bid = playerDb.bid,
      folded = playerDb.folded,
      busted = playerDb.busted,
      hole = playerDb.hole,
      isCreator = playerDb.isCreator,
    )
  }

  def spectatorFromDb(playerDb: PlayerDb): Spectator = {
    Spectator(
      gameId = GameId(playerDb.gameId),
      playerId = PlayerId(playerDb.playerId),
      playerAddress = PlayerAddress(playerDb.playerAddress),
      playerKey = PlayerKey(playerDb.playerKey),
    )
  }

  private def lookupPlayerDb(gameId: String, playerDbs: List[PlayerDb])(playerId: String): Either[Failures, PlayerDb] = {
    playerDbs
      .find(_.playerId == playerId)
      .toRight {
        Failures(
          s"Player $playerId not found in database for game $gameId",
          s"Could not load all players",
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

  def roundWinnings(game: Game, player: Player, results: List[Result]): RoundWinnings = {
    RoundWinnings(
      self = summariseSelf(player),
      game = summariseGame(game),
      results = results.map(summariseResult)
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
          hands = Play.hands(players)
        )
    }
  }

  def summariseGame(game: Game): GameSummary = {
    GameSummary(
      gameId = game.gameId,
      gameName = game.gameName,
      players = game.players.map(summarisePlayer),
      spectators = game.spectators.map(summariseSpectator),
      round = summariseRound(game.round, game.players),
      inTurn = game.inTurn.map(summarisePlayer),
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
      bid = player.bid,
      folded = player.folded,
      busted = player.busted,
      hole = player.hole,
    )
  }

  def summarisePlayer(player: Player): PlayerSummary = {
    PlayerSummary(
      playerId = player.playerId,
      screenName = player.screenName,
      stack = player.stack,
      pot = player.pot,
      bid = player.bid,
      folded = player.folded,
      busted = player.busted,
    )
  }

  def summariseSpectator(spectator: Spectator): SpectatorSummary = {
    SpectatorSummary(
      playerId = spectator.playerId,
    )
  }

  def summariseResult(result: Result): ResultSummary = {
    ResultSummary(
      player = summarisePlayer(result.player),
      hand = result.hand,
      winnings = result.winnings,
    )
  }
}
