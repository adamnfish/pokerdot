module Logic exposing (gameIsFinished, isBusted, winner)

import List.Extra
import Model exposing (Game, Player, Round(..))


isBusted : Round -> Player -> Bool
isBusted round player =
    case round of
        PreFlopRound ->
            player.busted

        FlopRound _ _ _ ->
            player.busted

        TurnRound _ _ _ _ ->
            player.busted

        RiverRound _ _ _ _ _ ->
            player.busted

        ShowdownRound _ _ _ _ _ _ ->
            -- during the showdown the player's stack has been updated but their status is not yet busted
            player.busted || player.stack == 0


gameIsFinished : Game -> Bool
gameIsFinished game =
    -- not finished until there are 1 or fewer players that are not busted
    1 >= List.Extra.count (isBusted game.round >> not) game.players


winner : Game -> Maybe Player
winner game =
    case List.filter (isBusted game.round >> not) game.players of
        winningPlayer :: [] ->
            Just winningPlayer

        _ ->
            Nothing
