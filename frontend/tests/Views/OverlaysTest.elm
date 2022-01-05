module Views.OverlaysTest exposing (..)

import Expect
import Fixtures exposing (startTimeGenerator, todo)
import Logic exposing (..)
import Model exposing (GameId(..), PlayerId(..), Rank(..), Round(..), Suit(..), TimerLevel(..))
import Test exposing (Test, describe, fuzz, test)
import Time exposing (millisToPosix)
import Views.Overlays exposing (TimerLevelInfo, timerLevelsFromStatus)


all : Test
all =
    describe "overlays"
        [ describe "timerLevelsFromStatus"
            [ describe "calculates current level"
                [ fuzz startTimeGenerator "first level if the timer has not started" <|
                    \startTimeOffset ->
                        let
                            timerStatus =
                                { timerStartTime = millisToPosix startTimeOffset
                                , pausedTime = Nothing
                                , levels =
                                    [ RoundLevel 100 5
                                    , RoundLevel 100 10
                                    , RoundLevel 100 20
                                    ]
                                }

                            now =
                                millisToPosix <| startTimeOffset + (1 * 1000)

                            timerLevels =
                                timerLevelsFromStatus now timerStatus
                        in
                        timerLevels.current
                            |> Expect.equal (Just <| asTimerLevelInfo 0 <| RoundLevel 100 5)
                , fuzz startTimeGenerator "middle round if the timer has been running for a bit" <|
                    \startTimeOffset ->
                        let
                            timerStatus =
                                { timerStartTime = millisToPosix startTimeOffset
                                , pausedTime = Nothing
                                , levels =
                                    [ RoundLevel 100 5
                                    , RoundLevel 100 10
                                    , RoundLevel 100 20
                                    ]
                                }

                            now =
                                millisToPosix <| startTimeOffset + (180 * 1000)

                            timerLevels =
                                timerLevelsFromStatus now timerStatus
                        in
                        timerLevels.current
                            |> Expect.equal (Just <| asTimerLevelInfo 100 <| RoundLevel 100 10)
                , fuzz startTimeGenerator "can return a break round" <|
                    \startTimeOffset ->
                        let
                            timerStatus =
                                { timerStartTime = millisToPosix startTimeOffset
                                , pausedTime = Nothing
                                , levels =
                                    [ RoundLevel 100 5
                                    , RoundLevel 100 10
                                    , RoundLevel 100 20
                                    , BreakLevel 100
                                    , RoundLevel 100 40
                                    ]
                                }

                            now =
                                millisToPosix <| startTimeOffset + (320 * 1000)

                            timerLevels =
                                timerLevelsFromStatus now timerStatus
                        in
                        timerLevels.current
                            |> Expect.equal (Just <| asTimerLevelInfo 300 <| BreakLevel 100)
                , fuzz startTimeGenerator "Nothing if the timer has finished" <|
                    \startTimeOffset ->
                        let
                            timerStatus =
                                { timerStartTime = millisToPosix startTimeOffset
                                , pausedTime = Nothing
                                , levels =
                                    [ RoundLevel 100 5
                                    , RoundLevel 100 10
                                    , RoundLevel 100 20
                                    ]
                                }

                            now =
                                millisToPosix <| startTimeOffset + (9000 * 1000)

                            timerLevels =
                                timerLevelsFromStatus now timerStatus
                        in
                        timerLevels.current
                            |> Expect.equal Nothing
                , fuzz startTimeGenerator "returns the correct level when the timer is paused" <|
                    \startTimeOffset ->
                        let
                            timerStatus =
                                { timerStartTime = millisToPosix startTimeOffset
                                , pausedTime = Just <| millisToPosix (startTimeOffset + (140 * 1000))
                                , levels =
                                    [ RoundLevel 100 5
                                    , RoundLevel 100 10
                                    , RoundLevel 100 20
                                    ]
                                }

                            now =
                                millisToPosix <| startTimeOffset + (700 * 1000)

                            timerLevels =
                                timerLevelsFromStatus now timerStatus
                        in
                        timerLevels.current
                            |> Expect.equal (Just <| asTimerLevelInfo 100 <| RoundLevel 100 10)
                , fuzz startTimeGenerator "empty if the timer has finished" <|
                    \startTimeOffset ->
                        let
                            timerStatus =
                                { timerStartTime = millisToPosix startTimeOffset
                                , pausedTime = Nothing
                                , levels =
                                    [ RoundLevel 100 5
                                    , RoundLevel 100 10
                                    , RoundLevel 100 20
                                    ]
                                }

                            now =
                                millisToPosix <| startTimeOffset + (800 * 1000)

                            timerLevels =
                                timerLevelsFromStatus now timerStatus
                        in
                        timerLevels.current
                            |> Expect.equal Nothing
                , fuzz startTimeGenerator "returns nothing if the timer starts in the future 🤷️" <|
                    \startTimeOffset ->
                        let
                            timerStatus =
                                { timerStartTime = millisToPosix startTimeOffset
                                , pausedTime = Nothing
                                , levels =
                                    [ RoundLevel 100 5
                                    , RoundLevel 100 10
                                    , RoundLevel 100 20
                                    ]
                                }

                            -- before the timer started!
                            now =
                                millisToPosix <| startTimeOffset - (100 * 1000)

                            timerLevels =
                                timerLevelsFromStatus now timerStatus
                        in
                        timerLevels.current
                            |> Expect.equal Nothing
                ]
            ]
        , describe "calculates previous levels"
            [ fuzz startTimeGenerator "empty if we're still on the first timer level" <|
                \startTimeOffset ->
                    let
                        timerStatus =
                            { timerStartTime = millisToPosix startTimeOffset
                            , pausedTime = Nothing
                            , levels =
                                [ RoundLevel 100 5
                                , RoundLevel 100 10
                                , RoundLevel 100 20
                                , BreakLevel 100
                                , RoundLevel 100 40
                                ]
                            }

                        now =
                            millisToPosix <| startTimeOffset + 1

                        timerLevels =
                            timerLevelsFromStatus now timerStatus
                    in
                    timerLevels.previous
                        |> Expect.equal []
            , fuzz startTimeGenerator "just the first level if we're on the second" <|
                \startTimeOffset ->
                    let
                        timerStatus =
                            { timerStartTime = millisToPosix startTimeOffset
                            , pausedTime = Nothing
                            , levels =
                                [ RoundLevel 100 5
                                , RoundLevel 100 10
                                , RoundLevel 100 20
                                , BreakLevel 100
                                , RoundLevel 100 40
                                ]
                            }

                        now =
                            millisToPosix <| startTimeOffset + 1

                        timerLevels =
                            timerLevelsFromStatus now timerStatus
                    in
                    timerLevels.previous
                        |> Expect.equal []
            , fuzz startTimeGenerator "multiple levels if we're a ways through" <|
                \startTimeOffset ->
                    let
                        timerStatus =
                            { timerStartTime = millisToPosix startTimeOffset
                            , pausedTime = Nothing
                            , levels =
                                [ RoundLevel 100 5
                                , RoundLevel 100 10
                                , RoundLevel 100 20
                                , BreakLevel 100
                                , RoundLevel 100 40
                                ]
                            }

                        now =
                            millisToPosix <| startTimeOffset + (310 * 1000)

                        timerLevels =
                            timerLevelsFromStatus now timerStatus
                    in
                    timerLevels.previous
                        |> Expect.equal
                            [ asTimerLevelInfo 0 <| RoundLevel 100 5
                            , asTimerLevelInfo 100 <| RoundLevel 100 10
                            , asTimerLevelInfo 200 <| RoundLevel 100 20
                            ]
            , fuzz startTimeGenerator "all levels if the timer is finished" <|
                \startTimeOffset ->
                    let
                        timerStatus =
                            { timerStartTime = millisToPosix startTimeOffset
                            , pausedTime = Nothing
                            , levels =
                                [ RoundLevel 100 5
                                , RoundLevel 100 10
                                , RoundLevel 100 20
                                , BreakLevel 100
                                , RoundLevel 100 40
                                ]
                            }

                        now =
                            millisToPosix <| startTimeOffset + (9000 * 1000)

                        timerLevels =
                            timerLevelsFromStatus now timerStatus
                    in
                    timerLevels.previous
                        |> Expect.equal
                            [ asTimerLevelInfo 0 <| RoundLevel 100 5
                            , asTimerLevelInfo 100 <| RoundLevel 100 10
                            , asTimerLevelInfo 200 <| RoundLevel 100 20
                            , asTimerLevelInfo 300 <| BreakLevel 100
                            , asTimerLevelInfo 400 <| RoundLevel 100 40
                            ]
            ]
        , describe "calculates future levels"
            [ fuzz startTimeGenerator "all but one level if we've just started" <|
                \startTimeOffset ->
                    let
                        timerStatus =
                            { timerStartTime = millisToPosix startTimeOffset
                            , pausedTime = Nothing
                            , levels =
                                [ RoundLevel 100 5
                                , RoundLevel 100 10
                                , RoundLevel 100 20
                                , BreakLevel 100
                                , RoundLevel 100 40
                                ]
                            }

                        now =
                            millisToPosix <| startTimeOffset + (1 * 1000)

                        timerLevels =
                            timerLevelsFromStatus now timerStatus
                    in
                    timerLevels.future
                        |> Expect.equal
                            [ asTimerLevelInfo 100 <| RoundLevel 100 10
                            , asTimerLevelInfo 200 <| RoundLevel 100 20
                            , asTimerLevelInfo 300 <| BreakLevel 100
                            , asTimerLevelInfo 400 <| RoundLevel 100 40
                            ]
            , fuzz startTimeGenerator "fewer levels if we're a ways through" <|
                \startTimeOffset ->
                    let
                        timerStatus =
                            { timerStartTime = millisToPosix startTimeOffset
                            , pausedTime = Nothing
                            , levels =
                                [ RoundLevel 100 5
                                , RoundLevel 100 10
                                , RoundLevel 100 20
                                , BreakLevel 100
                                , RoundLevel 100 40
                                ]
                            }

                        now =
                            millisToPosix <| startTimeOffset + (210 * 1000)

                        timerLevels =
                            timerLevelsFromStatus now timerStatus
                    in
                    timerLevels.future
                        |> Expect.equal
                            [ asTimerLevelInfo 300 <| BreakLevel 100
                            , asTimerLevelInfo 400 <| RoundLevel 100 40
                            ]
            , fuzz startTimeGenerator "empty if the timer is finished" <|
                \startTimeOffset ->
                    let
                        timerStatus =
                            { timerStartTime = millisToPosix startTimeOffset
                            , pausedTime = Nothing
                            , levels =
                                [ RoundLevel 100 5
                                , RoundLevel 100 10
                                , RoundLevel 100 20
                                , BreakLevel 100
                                , RoundLevel 100 40
                                ]
                            }

                        now =
                            millisToPosix <| startTimeOffset + (9000 * 1000)

                        timerLevels =
                            timerLevelsFromStatus now timerStatus
                    in
                    timerLevels.future
                        |> Expect.equal []
            ]
        ]


asTimerLevelInfo : Int -> TimerLevel -> TimerLevelInfo
asTimerLevelInfo startTimeOffset timerLevel =
    { level = timerLevel
    , startTimeOffset = startTimeOffset
    }
