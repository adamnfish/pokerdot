module TimerTests exposing (all)

import Expect
import Fuzz exposing (intRange)
import Model exposing (TimerLevel(..))
import Test exposing (Test, describe, fuzz, only, test)
import Time exposing (millisToPosix)
import Views.Timers exposing (CurrentTimerLevel(..), currentTimerLevel)


startTimeGenerator =
    intRange 0 100000


all : Test
all =
    describe "timers"
        [ describe "currentTimerLevel"
            [ fuzz startTimeGenerator "calculates the correct blind amount for a running timer" <|
                \startTime ->
                    let
                        timerStatus =
                            { timerStartTime = millisToPosix startTime
                            , pausedTime = Nothing
                            , levels =
                                [ RoundLevel 100 10
                                , RoundLevel 100 20
                                , BreakLevel 100
                                , RoundLevel 100 50
                                ]
                            }

                        now =
                            millisToPosix <| startTime + (150 * 1000)
                    in
                    currentTimerLevel timerStatus now
                        |> Expect.equal
                            (TimerRunning
                                { levelDuration = 100, levelProgress = 50, smallBlind = 20 }
                                (Just <| BreakLevel 100)
                            )
            , fuzz startTimeGenerator "takes the last blind amount for a finished timer" <|
                \startTime ->
                    let
                        timerStatus =
                            { timerStartTime = millisToPosix startTime
                            , pausedTime = Nothing
                            , levels =
                                [ RoundLevel 100 10
                                , RoundLevel 100 20
                                , BreakLevel 100
                                , RoundLevel 100 50
                                ]
                            }

                        now =
                            millisToPosix <| startTime + (600 * 1000)
                    in
                    currentTimerLevel timerStatus now
                        |> Expect.equal (TimerFinished 50)
            , fuzz startTimeGenerator "calculates the correct blind amount if the timer is paused" <|
                \startTime ->
                    let
                        timerStatus =
                            { timerStartTime = millisToPosix startTime
                            , pausedTime = Just <| millisToPosix <| startTime + (120 * 1000)
                            , levels =
                                [ RoundLevel 100 10
                                , RoundLevel 100 20
                                , BreakLevel 100
                                , RoundLevel 100 50
                                ]
                            }

                        now =
                            millisToPosix <| startTime + (900 * 1000)
                    in
                    currentTimerLevel timerStatus now
                        |> Expect.equal
                            (TimerPaused
                                { levelDuration = 100, levelProgress = 20, smallBlind = 20 }
                                (Just <| BreakLevel 100)
                            )
            , fuzz startTimeGenerator "calculates the correct blind amount if the timer is paused when finished" <|
                \startTime ->
                    let
                        timerStatus =
                            { timerStartTime = millisToPosix startTime
                            , pausedTime = Just <| millisToPosix <| startTime + (850 * 1000)
                            , levels =
                                [ RoundLevel 100 10
                                , RoundLevel 100 20
                                , BreakLevel 100
                                , RoundLevel 100 50
                                ]
                            }

                        now =
                            millisToPosix <| startTime + (1200 * 1000)
                    in
                    currentTimerLevel timerStatus now
                        |> Expect.equal (TimerPausedFinish 50)
            , fuzz startTimeGenerator "takes the last valid blind amount if we're on a break" <|
                \startTime ->
                    let
                        timerStatus =
                            { timerStartTime = millisToPosix startTime
                            , pausedTime = Nothing
                            , levels =
                                [ RoundLevel 100 10
                                , RoundLevel 100 20
                                , BreakLevel 100
                                , RoundLevel 100 50
                                ]
                            }

                        now =
                            millisToPosix <| startTime + (230 * 1000)
                    in
                    currentTimerLevel timerStatus now
                        |> Expect.equal
                            (TimerBreak
                                { levelDuration = 100, levelProgress = 30, smallBlind = 20 }
                                (Just <| RoundLevel 100 50)
                            )
            , fuzz startTimeGenerator "takes the last valid blind amount if we're paused during a break" <|
                \startTime ->
                    let
                        timerStatus =
                            { timerStartTime = millisToPosix startTime
                            , pausedTime = Just <| millisToPosix <| startTime + (260 * 1000)
                            , levels =
                                [ RoundLevel 100 10
                                , RoundLevel 100 20
                                , BreakLevel 100
                                , RoundLevel 100 50
                                ]
                            }

                        now =
                            millisToPosix <| startTime + (1000 * 1000)
                    in
                    currentTimerLevel timerStatus now
                        |> Expect.equal
                            (TimerPausedBreak
                                { levelDuration = 100, levelProgress = 60, smallBlind = 20 }
                                (Just <| RoundLevel 100 50)
                            )
            ]
        ]