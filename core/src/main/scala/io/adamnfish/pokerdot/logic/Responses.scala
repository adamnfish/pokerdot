package io.adamnfish.pokerdot.logic

import io.adamnfish.pokerdot.models._


object Responses {
  def welcome(game: Game, newPlayer: Player): Response[Welcome] = {
    val welcomeMessage = Welcome(newPlayer.playerKey, newPlayer.playerId, game.gameId, game.gameName, newPlayer.screenName)
    val action = PlayerJoinedSummary(
      Representations.summarisePlayer(newPlayer)
    )
    messageAndStatuses(welcomeMessage, newPlayer.playerAddress, game, action)
  }

  def gameStatuses(game: Game, actionSummary: ActionSummary): Response[GameStatus] = {
    Response(
      Map.empty,
      game.players.map { player =>
        player.playerAddress -> Representations.gameStatus(game, player, actionSummary)
      }.toMap,
    )
  }

  def roundWinnings(game: Game, results: List[Result]): Response[RoundWinnings] = {
    Response(
      game.players.map { player =>
        player.playerAddress -> Representations.roundWinnings(game, player, results)
      }.toMap,
      Map.empty,
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
      ),
      Map.empty
    )
  }

  def ok(playerAddress: PlayerAddress): Response[Status] = {
    Response(
      Map(
        playerAddress -> Status("ok")
      ),
      Map.empty
    )
  }

  def tbd[A <: Message](): Response[A] = {
    ???
  }
}
