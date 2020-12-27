package io.adamnfish.pokerdot.logic

import io.adamnfish.pokerdot.logic.Representations.summariseGame
import io.adamnfish.pokerdot.models._


object Responses {
  def welcome(game: Game, newPlayer: Player): Response[Welcome] = {
    val gameSummary = summariseGame(game)
    val welcomeMessage = Welcome(
      newPlayer.playerKey,
      newPlayer.playerId,
      game.gameId,
      game.gameName,
      newPlayer.screenName,
      spectator = false,
      game = gameSummary,
    )
    val action = PlayerJoinedSummary(
      Representations.summarisePlayer(newPlayer)
    )
    messageAndStatuses(welcomeMessage, newPlayer.playerAddress, game, action)
  }

  def gameStatuses(game: Game, actionSummary: ActionSummary): Response[GameStatus] = {
    Response(
      game.players.map { player =>
        player.playerAddress -> Representations.gameStatus(game, player, actionSummary)
      }.toMap,
    )
  }

  /**
   * Winnings needs to be provided:
   * - potWinnings (1 entry per side pot and one entry for the main pot)
   * - playerWinnings (1 entry per player)
   */
  def roundWinnings(game: Game, potWinnings: List[PotWinnings], playerWinnings: List[PlayerWinnings]): Response[RoundWinnings] = {
    Response(
      game.players.map { player =>
        player.playerAddress -> Representations.roundWinnings(game, player, potWinnings, playerWinnings)
      }.toMap,
    )
  }

  def messageAndStatuses[A <: Message](message: A, playerAddress: PlayerAddress, game: Game, actionSummary: ActionSummary): Response[A] = {
    val response = gameStatuses(game, actionSummary)
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
      )
    )
  }

  def ok(playerAddress: PlayerAddress): Response[Status] = {
    Response(
      Map(
        playerAddress -> Status("ok")
      )
    )
  }

  def tbd[A <: Message](): Response[A] = {
    ???
  }
}
