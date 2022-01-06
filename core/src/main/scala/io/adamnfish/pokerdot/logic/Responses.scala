package io.adamnfish.pokerdot.logic

import io.adamnfish.pokerdot.logic.Representations.{summariseGame, summariseSelf}
import io.adamnfish.pokerdot.models._


object Responses {
  def welcome(game: Game, newPlayer: Player, newPlayerAddress: PlayerAddress): Response[Welcome] = {
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
    val withAllStatuses = messageAndStatuses(welcomeMessage, newPlayer.playerId, newPlayerAddress, game, action)
    withAllStatuses.copy(
      // we don't want to send a status message to the new player
      statuses = withAllStatuses.statuses.filterNot { case (address, _) => address == newPlayerAddress }
    )
  }

  /**
   * Send game status updates to all players.
   *
   * The current player's active address should be used, rather than the one that is saved in the DB.
   */
  def gameStatuses(game: Game, actionSummary: ActionSummary, playerId: PlayerId, playerAddress: PlayerAddress): Response[GameStatus] = {
    Response(
      Map.empty,
      game.players.map { player =>
        val address =
          if (player.playerId == playerId) playerAddress
          else player.playerAddress
        address -> Representations.gameStatus(game, player, actionSummary)
      }.toMap,
    )
  }

  /**
   * Winnings needs to be provided:
   * - potWinnings (1 entry per side pot and one entry for the main pot)
   * - playerWinnings (1 entry per player)
   *
   * The current player's active address should be used, rather than the one that is saved in the DB.
   */
  def roundWinnings(game: Game, potWinnings: List[PotWinnings], playerWinnings: List[PlayerWinnings], playerId: PlayerId, playerAddress: PlayerAddress): Response[RoundWinnings] = {
    Response(
      game.players.map { player =>
        val address =
          if (player.playerId == playerId) playerAddress
          else player.playerAddress
        address -> Representations.roundWinnings(game, player, potWinnings, playerWinnings)
      }.toMap,
      Map.empty,
    )
  }

  private def messageAndStatuses[A <: Message](message: A, playerId: PlayerId, playerAddress: PlayerAddress, game: Game, actionSummary: ActionSummary): Response[A] = {
    val response = gameStatuses(game, actionSummary, playerId: PlayerId, playerAddress: PlayerAddress)
    response.copy(
      messages = Map(
        playerAddress -> message
      )
    )
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
