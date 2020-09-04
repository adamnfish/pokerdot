package io.adamnfish.pokerdot.logic

import io.adamnfish.pokerdot.models.{Game, GameId, Player}


object Games {
  def gameCode(gameId: GameId): String = {
    gameId.gid.take(4)
  }
}
