module Views.Timers exposing (CurrentTimerLevel(..), TimerSpeed(..), currentTimerLevel, defaultStack, defaultTimerLevels, filteredTimerLevels, smallBlindIsSmallEnough, timerRecommendations)

import List.Extra
import Maybe.Extra
import Model exposing (Msg, TimerLevel(..), TimerStatus)
import Time exposing (Posix, posixToMillis)


type TimerSpeed
    = LongGame
    | MediumGame
    | ShortGame


timerRecommendations : TimerSpeed -> Int -> Int -> List TimerLevel
timerRecommendations timerSpeed playerCount stack =
    -- NOTE: expects stacks of at least 100
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
                    , 7
                    , 10
                    , 15
                    , 25
                    , 50
                    , 75
                    , 100
                    , 150
                    , 200
                    , 350
                    , 500
                    , 750
                    , 1000
                    , 1500
                    , 2000
                    , 3000
                    , 5000
                    , 7500
                    , 10000
                    , 15000
                    , 20000
                    , 30000
                    , 40000
                    , 50000
                    ]

                MediumGame ->
                    [ 1
                    , 2
                    , 3
                    , 6
                    , 10
                    , 20
                    , 50
                    , 75
                    , 100
                    , 150
                    , 200
                    , 500
                    , 1000
                    , 1500
                    , 2000
                    , 5000
                    , 7500
                    , 10000
                    , 15000
                    , 20000
                    , 30000
                    , 50000
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
                    , 500
                    , 1000
                    , 2000
                    , 5000
                    , 10000
                    , 20000
                    , 50000
                    ]
    in
    List.map
        (\multiple ->
            RoundLevel duration <| multiple * initialSmallBlind
        )
        blindMultiples


smallBlindIsSmallEnough : Int -> Int -> Int -> Bool
smallBlindIsSmallEnough stackSize playerCount candidateSmallBlind =
    (4 * candidateSmallBlind) < stackSize * playerCount


filteredTimerLevels : Int -> Int -> List TimerLevel -> List TimerLevel
filteredTimerLevels stackSize playerCount =
    List.Extra.takeWhile
        (\level ->
            case level of
                RoundLevel _ smallBlind ->
                    smallBlindIsSmallEnough stackSize playerCount smallBlind

                BreakLevel _ ->
                    True
        )


defaultTimerLevels playerCount =
    timerRecommendations ShortGame playerCount 100


defaultStack =
    100


type alias CurrentTimerLevelInfo =
    { levelDuration : Int
    , levelProgress : Int
    , smallBlind : Int
    }


type
    CurrentTimerLevel
    -- Describes current and next status of a running timer
    = TimerRunning CurrentTimerLevelInfo (Maybe TimerLevel)
    | TimerBreak CurrentTimerLevelInfo (Maybe TimerLevel)
    | TimerFinished Int
    | TimerPaused CurrentTimerLevelInfo (Maybe TimerLevel)
    | TimerPausedBreak CurrentTimerLevelInfo (Maybe TimerLevel)
    | TimerPausedFinish Int


currentTimerLevel : Posix -> TimerStatus -> CurrentTimerLevel
currentTimerLevel now timerStatus =
    let
        timerProgress =
            case timerStatus.pausedTime of
                Just pausedTime ->
                    ceiling <| toFloat (posixToMillis pausedTime - posixToMillis timerStatus.timerStartTime) / 1000

                Nothing ->
                    ceiling <| toFloat (posixToMillis now - posixToMillis timerStatus.timerStartTime) / 1000

        loop : List TimerLevel -> Int -> Maybe TimerLevel -> Maybe ( TimerLevel, Int ) -> Maybe TimerLevel -> CurrentTimerLevel
        loop levels levelStartTime maybePrev maybeCurrent maybeNext =
            case levels of
                level :: tail ->
                    let
                        currentLevelDuration =
                            case level of
                                RoundLevel duration _ ->
                                    duration

                                BreakLevel duration ->
                                    duration
                    in
                    if levelStartTime < timerProgress && levelStartTime + currentLevelDuration < timerProgress then
                        -- haven't got to the current level, so this is at best "previous"
                        loop tail (levelStartTime + currentLevelDuration) (Just level) Nothing Nothing

                    else if levelStartTime < timerProgress && levelStartTime + currentLevelDuration > timerProgress then
                        -- this is the current level, set current and fix prev
                        -- TODO: only set prev if it is a round (not a break)
                        let
                            progress =
                                timerProgress - levelStartTime
                        in
                        loop tail (levelStartTime + currentLevelDuration) maybePrev (Just ( level, progress )) Nothing

                    else
                        -- this is a subsequent level, set next if it does not already exist
                        -- loop with empty list to stop us having to look at the rest of the levels
                        loop [] (levelStartTime + currentLevelDuration) maybePrev maybeCurrent <|
                            case maybeNext of
                                Nothing ->
                                    Just level

                                Just next ->
                                    Just next

                [] ->
                    case ( maybePrev, maybeCurrent ) of
                        ( _, Just ( RoundLevel duration smallBlind, progress ) ) ->
                            if Maybe.Extra.isJust timerStatus.pausedTime then
                                TimerPaused
                                    { levelDuration = duration
                                    , levelProgress = progress
                                    , smallBlind = smallBlind
                                    }
                                    maybeNext

                            else
                                TimerRunning
                                    { levelDuration = duration
                                    , levelProgress = progress
                                    , smallBlind = smallBlind
                                    }
                                    maybeNext

                        ( Just (RoundLevel _ prevSmallBlind), Just ( BreakLevel duration, progress ) ) ->
                            if Maybe.Extra.isJust timerStatus.pausedTime then
                                TimerPausedBreak
                                    { levelDuration = duration
                                    , levelProgress = progress
                                    , smallBlind = prevSmallBlind
                                    }
                                    maybeNext

                            else
                                TimerBreak
                                    { levelDuration = duration
                                    , levelProgress = progress
                                    , smallBlind = prevSmallBlind
                                    }
                                    maybeNext

                        ( Just (RoundLevel _ prevSmallBlind), Nothing ) ->
                            -- timer has finished
                            if Maybe.Extra.isJust timerStatus.pausedTime then
                                TimerPausedFinish prevSmallBlind

                            else
                                TimerFinished prevSmallBlind

                        ( Nothing, Nothing ) ->
                            -- must have been empty timer levels, which is validated against elsewhere
                            TimerFinished 0

                        _ ->
                            -- TODO: expand on these cases
                            TimerFinished 1
    in
    loop timerStatus.levels 0 Nothing Nothing Nothing
