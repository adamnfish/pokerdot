module Views.Timers exposing (defaultStack, defaultTimerLevels, timerUi)

import Element exposing (..)
import Element.Font as Font
import Model exposing (Msg, TimerLevel(..))
import Views.Elements exposing (controlsButton)
import Views.Theme as Theme



{-
   Useful resource!
     https://www.pokereagles.com/home-poker/tournament-blinds.php
-}


type TimerSpeed
    = LongGame
    | MediumGame
    | ShortGame


recommendations : TimerSpeed -> Int -> Int -> List TimerLevel
recommendations timerSpeed playerCount stack =
    -- expects stacks of at least 100
    -- TODO: use total funds to cap levels appropriately
    -- TODO: optionally insert breaks
    let
        initialSmallBlind =
            stack // 100

        totalFunds =
            playerCount * stack

        duration =
            case timerSpeed of
                LongGame ->
                    15 * 60

                MediumGame ->
                    10 * 60

                ShortGame ->
                    10 * 60

        blindMultiples =
            case timerSpeed of
                LongGame ->
                    [ 1
                    , 2
                    , 3
                    , 5
                    , 10
                    , 15
                    , 25
                    , 50
                    , 75
                    , 100
                    , 150
                    , 200

                    -- adjust the end based on player count
                    ]

                MediumGame ->
                    [ 1
                    , 2
                    , 5
                    , 10
                    , 20
                    , 50
                    , 75
                    , 100
                    , 150
                    , 200

                    -- adjust the end based on player count
                    ]

                ShortGame ->
                    [ 1
                    , 2
                    , 5
                    , 10
                    , 20
                    , 50
                    , 100
                    , 200

                    -- adjust the end based on player count
                    , 400 -- requires at least 4 players at game's start
                    ]
    in
    List.map
        (\multiple ->
            RoundLevel duration <| multiple * initialSmallBlind
        )
        blindMultiples


timerLevelUi : TimerLevel -> Element Msg
timerLevelUi timerLevel =
    -- TODO: controls for deletion, addition and movement
    let
        durationEl duration =
            -- TODO: human formatting of times
            row
                []
                [ text <| String.fromInt duration ]
    in
    case timerLevel of
        RoundLevel duration smallBlind ->
            row
                [ width fill ]
                [ durationEl duration
                , text <| String.fromInt smallBlind
                , text " / "
                , text <| String.fromInt <| smallBlind * 2
                , el
                    [ alignRight ]
                  <|
                    text "x"
                ]

        BreakLevel duration ->
            row
                [ width fill ]
                [ text "break"
                , durationEl duration
                , el
                    [ alignRight ]
                  <|
                    text "x"
                ]


timerUi : (List TimerLevel -> Msg) -> List TimerLevel -> Int -> Int -> Element Msg
timerUi msg timerLevels playerCount stack =
    -- show recommendation buttons
    -- and current choice
    -- controls to change all round lengths at once
    let
        longGameTimer =
            recommendations LongGame playerCount stack

        mediumGameTimer =
            recommendations MediumGame playerCount stack

        shortGameTimer =
            recommendations ShortGame playerCount stack
    in
    column
        [ width fill ]
        [ wrappedRow
            [ width fill
            , spacing 8
            ]
            [ controlsButton Theme.scheme1 (msg shortGameTimer) <|
                column
                    [ spacing 5
                    , width fill
                    ]
                    [ el
                        [ width fill
                        , Font.center
                        ]
                      <|
                        text "short"
                    , el
                        [ width fill
                        , Font.center
                        ]
                      <|
                        text "game"
                    ]
            , controlsButton Theme.scheme1 (msg mediumGameTimer) <|
                column
                    [ spacing 5
                    , width fill
                    ]
                    [ el
                        [ width fill
                        , Font.center
                        ]
                      <|
                        text "medium"
                    , el
                        [ width fill
                        , Font.center
                        ]
                      <|
                        text "game"
                    ]
            , controlsButton Theme.scheme1 (msg longGameTimer) <|
                column
                    [ spacing 5
                    , width fill
                    ]
                    [ el
                        [ width fill
                        , Font.center
                        ]
                      <|
                        text "long"
                    , el
                        [ width fill
                        , Font.center
                        ]
                      <|
                        text "game"
                    ]
            ]
        , column
            [ width fill ]
          <|
            List.map timerLevelUi timerLevels
        ]


defaultTimerLevels playerCount stack =
    recommendations ShortGame playerCount stack


defaultStack =
    1000
