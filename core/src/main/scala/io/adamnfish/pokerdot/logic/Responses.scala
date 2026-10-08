package io.adamnfish.pokerdot.logic

import io.adamnfish.pokerdot.logic.Representations.{summariseGame, summariseSelf}
import io.adamnfish.pokerdot.models._


object Responses {
  def welcome(game: Game, newPlayer: Player, newPlayerAddress: PlayerAddress, addresses: Map[PlayerId, Set[PlayerAddress]]): Response[Welcome] = {
    val gameSummary = summariseGame(game)
    val welcomeMessage = Welcome(
      newPlayer.playerKey,
      newPlayer.playerId,
      game.gameId,
      game.gameCode,
      game.gameName,
      newPlayer.screenName,
      spectator = false,
      game = gameSummary,
      self = summariseSelf(newPlayer)
    )
    val action = PlayerJoinedSummary(newPlayer.playerId)
    val statuses = gameStatuses(game, action, addresses).statuses
    Response(
      Map(newPlayerAddress -> welcomeMessage),
      // we don't want to send a status message to the new player
      statuses.filterNot { case (address, _) => address == newPlayerAddress },
    )
  }

  /**
   * Each player's addresses, from the game's connections.
   *
   * Always includes the requester's current address, which takes precedence over any other player's stale connection.
   */
  def playerAddresses(connections: List[ConnectionDb], playerId: PlayerId, playerAddress: PlayerAddress): Map[PlayerId, Set[PlayerAddress]] = {
    val connectionAddresses = connections
      .filterNot(_.address == playerAddress.address)
      .groupMap(c => PlayerId(c.playerId))(c => PlayerAddress(c.address))
      .view.mapValues(_.toSet).toMap
    connectionAddresses.updated(playerId, connectionAddresses.getOrElse(playerId, Set.empty) + playerAddress)
  }

  /**
   * Send game status updates to all of each player's addresses.
   */
  def gameStatuses(game: Game, actionSummary: ActionSummary, addresses: Map[PlayerId, Set[PlayerAddress]]): Response[GameStatus] = {
    Response(
      Map.empty,
      fanOut(game, addresses)(player => Representations.gameStatus(game, player, actionSummary)),
    )
  }

  /**
   * Winnings needs to be provided:
   * - potWinnings (1 entry per side pot and one entry for the main pot)
   * - playerWinnings (1 entry per player)
   */
  def roundWinnings(game: Game, potWinnings: List[PotWinnings], playerWinnings: List[PlayerWinnings], addresses: Map[PlayerId, Set[PlayerAddress]]): Response[RoundWinnings] = {
    Response(
      fanOut(game, addresses)(player => Representations.roundWinnings(game, player, potWinnings, playerWinnings)),
      Map.empty,
    )
  }

  private def fanOut[A](game: Game, addresses: Map[PlayerId, Set[PlayerAddress]])(message: Player => A): Map[PlayerAddress, A] = {
    game.players.flatMap { player =>
      val playerMessage = message(player)
      addresses.getOrElse(player.playerId, Set.empty).map(_ -> playerMessage)
    }.toMap
  }

  def justRespond[A <: Message](msg: A, playerAddress: PlayerAddress): Response[A] = {
    Response(
      Map(
        playerAddress -> msg
      ),
      Map.empty,
    )
  }

  def ok(playerAddress: PlayerAddress): Response[Status] = {
    justRespond(Status("ok"), playerAddress)
  }

  def tbd[A <: Message](): Response[A] = {
    ???
  }
}
