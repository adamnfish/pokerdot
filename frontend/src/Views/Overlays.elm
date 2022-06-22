module Views.Overlays exposing (..)

import Browser.Dom exposing (Viewport)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import FontAwesome.Attributes
import FontAwesome.Icon
import FontAwesome.Solid
import Maybe.Extra
import Model exposing (EditBlindsSettings(..), Msg(..), TimerLevel(..), TimerStatus)
import Time exposing (Posix, millisToPosix, posixToMillis)
import Timers exposing (TimerSpeed(..), filteredTimerLevels, timerRecommendations)
import Utils exposing (millisToTimeComponents)
import Views.Elements exposing (controlsButton, divider, formatBlinds, formatTimeComponent, pdTab, pokerdotInText, zWidths)
import Views.Theme as Theme


overlayTemplate : Viewport -> String -> Element Msg -> Element Msg
overlayTemplate viewport title overlayBody =
    el
        [ height fill
        , padding 16
        , width <| maximum (round viewport.viewport.width) <| px 520
        , centerX
        ]
    <|
        column
            [ width fill
            , height fill
            , padding 12
            , spacing 16
            , Background.color Theme.colours.primary
            , Border.color Theme.colours.night
            , Border.width 8
            , Border.shadow
                { offset = ( 2, 2 )
                , size = 0.75
                , blur = 2
                , color = Theme.colours.shadow
                }
            ]
            [ row
                [ width fill ]
                [ el
                    [ paddingXY 8 2
                    , Background.color Theme.colours.highlightPrimary
                    , Font.alignLeft
                    , Font.color <| Theme.textColour Theme.colours.lowlight
                    , Font.bold
                    ]
                  <|
                    text title
                , el
                    [ alignRight ]
                  <|
                    Input.button
                        [ padding 6
                        , Background.color Theme.colours.secondary
                        , Font.size 20
                        , Font.color <| Theme.textColour Theme.colours.white
                        , Border.rounded 2
                        , Border.solid
                        , Border.width 2
                        , Border.color Theme.colours.black
                        , Border.shadow
                            { offset = ( 5, 5 )
                            , size = 0
                            , blur = 0
                            , color = Theme.glow Theme.colours.secondary
                            }
                        , focused
                            [ Background.color <| Theme.focusColour Theme.colours.secondary
                            , Border.color Theme.colours.white
                            , Border.shadow
                                { offset = ( 5, 5 )
                                , size = 0
                                , blur = 0
                                , color = Theme.glow <| Theme.focusColour Theme.colours.secondary
                                }
                            ]
                        , mouseOver
                            [ Background.color <| Theme.focusColour Theme.colours.secondary ]
                        ]
                        { onPress = Just CloseOverlay
                        , label =
                            html <|
                                (FontAwesome.Solid.windowClose
                                    |> FontAwesome.Icon.present
                                    |> FontAwesome.Icon.styled [ FontAwesome.Attributes.sm ]
                                    |> FontAwesome.Icon.withId "pokerdot-overlay-close_pokerdot"
                                    |> FontAwesome.Icon.titled "close overlay"
                                    |> FontAwesome.Icon.view
                                )
                        }
                ]
            , overlayBody
            ]


helpOverlay : Element Msg
helpOverlay =
    let
        paragraphStyles =
            [ width fill
            , Font.alignLeft
            , Font.size 15
            ]
    in
    column
        [ width fill
        , spacing 8
        ]
        [ paragraph paragraphStyles
            [ pokerdotInText
            , text " provides a free online poker game for you and your friends. "
            , text "Specifically, "
            , pokerdotInText
            , text " uses the rules for no-limit Texas hold'em. "
            ]
        , paragraph paragraphStyles
            [ text "The aim of the game is to win everyone else's money." ]
        , paragraph paragraphStyles
            [ link
                [ Font.underline
                , Font.color Theme.colours.icon
                , Background.color Theme.colours.lowlight
                , paddingXY 2 0
                ]
                { url = "https://en.wikipedia.org/wiki/Texas_hold_%27em"
                , label = text "Wikipedia's article for Texas hold'em"
                }
            , text " includes a summary of the rules, but there are lots of resources online for learning poker. "
            ]
        , divider
        , paragraph paragraphStyles
            [ pokerdotInText
            , text " is open-source and free to play for non-commercial use. "
            , text " but is not licensed for commercial use."
            ]
        , paragraph paragraphStyles
            [ text "If you would like to use "
            , pokerdotInText
            , text " or its source-code for your business or expect to make any money from its use, "
            , text " you must get in touch with me to discuss your use. "
            , text " If you will be using "
            , pokerdotInText
            , text " for any virtuous purpose (in particular education and community support, charity, the climate crisis) "
            , text " please make sure to mention this when you contact me."
            ]
        ]


editBlindsOverlay : Posix -> Int -> Int -> Int -> Maybe TimerStatus -> EditBlindsSettings -> Element Msg
editBlindsOverlay now chipTotal playerCount gameSmallBlind maybeGameTimerStatus editBlindsSettings =
    column
        [ width fill
        , spacing 6
        , padding 8
        , Background.color Theme.colours.lowlight
        ]
        [ row
            [ spacing 8
            , moveUp 12
            ]
            [ pdTab Theme.colours.secondary
                (case editBlindsSettings of
                    DoNotEditBlinds ->
                        False

                    DoNotTrackBlinds ->
                        False

                    ManualBlinds _ ->
                        True

                    TimerBlinds _ ->
                        False
                )
                (InputUpdateBlindOverlay <| ManualBlinds gameSmallBlind)
                "manual blinds"
            , let
                tabTimerStatus =
                    case maybeGameTimerStatus of
                        Just timerStatus ->
                            TimerBlinds timerStatus

                        Nothing ->
                            TimerBlinds
                                { timerStartTime = now
                                , pausedTime = Nothing
                                , levels = []
                                }
              in
              pdTab Theme.colours.secondary
                (case editBlindsSettings of
                    DoNotEditBlinds ->
                        False

                    DoNotTrackBlinds ->
                        False

                    ManualBlinds _ ->
                        False

                    TimerBlinds _ ->
                        True
                )
                (InputUpdateBlindOverlay tabTimerStatus)
                "timer"
            ]
        , case editBlindsSettings of
            ManualBlinds currentSmallBlind ->
                let
                    recommended =
                        if currentSmallBlind == gameSmallBlind * 2 then
                            [ Element.none ]

                        else
                            [ el
                                [ alignLeft
                                ]
                              <|
                                controlsButton Theme.scheme1
                                    (UpdateBlindOverlay <| ManualBlinds (gameSmallBlind * 2))
                                <|
                                    column
                                        [ width fill
                                        , spacing 5
                                        ]
                                        [ el
                                            [ centerX ]
                                          <|
                                            text "update"
                                        , row
                                            [ centerX
                                            , Font.size 18
                                            ]
                                            [ text <| String.fromInt (gameSmallBlind * 2)
                                            , text " / "
                                            , text <| String.fromInt (gameSmallBlind * 4)
                                            ]
                                        ]

                            -- TODO: add a conditional second recommendation that rounds the blinds
                            ]
                in
                column
                    [ width fill
                    , spacing 4
                    ]
                    [ row
                        []
                        [ Input.text
                            [ width <| px 100
                            , paddingXY 10 8
                            , Border.solid
                            , Border.width 2
                            , Border.color Theme.colours.black
                            , Border.widthEach { zWidths | bottom = 2 }
                            , Border.rounded 0
                            , Background.color Theme.colours.white
                            , focused
                                [ Background.color Theme.colours.highlightPrimary
                                , Border.color Theme.colours.white
                                ]
                            , Font.alignRight
                            ]
                            { text = String.fromInt currentSmallBlind
                            , label =
                                Input.labelLeft
                                    [ width <| px 150
                                    , paddingXY 8 0
                                    , Font.alignRight
                                    , Font.color <| Theme.textColour Theme.colours.white
                                    ]
                                <|
                                    text "blind amount"
                            , placeholder =
                                Just <| Input.placeholder [] <| text "initial small blind"
                            , onChange =
                                \value ->
                                    let
                                        smallBlind =
                                            Maybe.withDefault 0 <| String.toInt value
                                    in
                                    InputUpdateBlindOverlay <|
                                        ManualBlinds smallBlind
                            }
                        , row
                            [ Font.color <| Theme.textColour Theme.colours.white ]
                            [ text " / "
                            , text <| String.fromInt (currentSmallBlind * 2)
                            ]
                        ]
                    , row
                        [ width fill
                        , padding 8
                        , spacing 12
                        ]
                        (List.append
                            recommended
                            [ el
                                [ alignRight
                                ]
                              <|
                                controlsButton Theme.scheme1
                                    (UpdateBlindOverlay <| ManualBlinds currentSmallBlind)
                                <|
                                    column
                                        [ width fill
                                        , spacing 5
                                        ]
                                        [ el
                                            [ centerX ]
                                          <|
                                            text "update"
                                        , row
                                            [ centerX
                                            , Font.size 18
                                            ]
                                            [ text <| String.fromInt currentSmallBlind
                                            , text " / "
                                            , text <| String.fromInt (currentSmallBlind * 2)
                                            ]
                                        ]
                            ]
                        )
                    ]

            TimerBlinds timerStatus ->
                let
                    willBePlaying =
                        Maybe.Extra.isJust timerStatus.pausedTime

                    ( playPauseButtonText, playPauseButtonIcon ) =
                        if willBePlaying then
                            ( "play", FontAwesome.Solid.play )

                        else
                            ( "pause", FontAwesome.Solid.pause )

                    playerStackShare =
                        chipTotal // playerCount

                    longGameTimerLevels =
                        timerRecommendations LongGame playerCount playerStackShare

                    mediumGameTimerLevels =
                        timerRecommendations MediumGame playerCount playerStackShare

                    shortGameTimerLevels =
                        timerRecommendations ShortGame playerCount playerStackShare

                    -- TODO: adjust timer start time to match the *blinds* rather than the total progress
                    longGameEditBlindsSettings =
                        TimerBlinds
                            { timerStartTime = timerStatus.timerStartTime
                            , pausedTime = timerStatus.pausedTime
                            , levels = filteredTimerLevels playerStackShare playerCount longGameTimerLevels
                            }

                    mediumGameEditBlindsSettings =
                        TimerBlinds
                            { timerStartTime = timerStatus.timerStartTime
                            , pausedTime = timerStatus.pausedTime
                            , levels = filteredTimerLevels playerStackShare playerCount mediumGameTimerLevels
                            }

                    shortGameEditBlindsSettings =
                        TimerBlinds
                            { timerStartTime = timerStatus.timerStartTime
                            , pausedTime = timerStatus.pausedTime
                            , levels = filteredTimerLevels playerStackShare playerCount shortGameTimerLevels
                            }

                    maybeNewTimer =
                        if Just timerStatus == maybeGameTimerStatus then
                            Nothing

                        else
                            Just timerStatus

                    timerLevels =
                        timerLevelsFromStatus now timerStatus
                in
                column
                    [ width fill
                    , spacing 12
                    ]
                    [ if Maybe.Extra.isJust maybeNewTimer then
                        Element.none

                      else
                        row
                            [ width fill ]
                            [ controlsButton Theme.scheme1
                                ToggleTimerPlayingOverlay
                              <|
                                column
                                    [ width fill
                                    , spacing 5
                                    ]
                                    [ el
                                        [ centerX ]
                                      <|
                                        text playPauseButtonText
                                    , el
                                        [ centerX ]
                                      <|
                                        html
                                            (playPauseButtonIcon
                                                |> FontAwesome.Icon.present
                                                |> FontAwesome.Icon.styled [ FontAwesome.Attributes.sm ]
                                                |> FontAwesome.Icon.view
                                            )
                                    ]
                            ]
                    , row
                        [ width fill ]
                        [ column
                            [ width fill
                            , spacing 2
                            ]
                            (List.concat
                                [ List.map (editTimerLevel now timerStatus.timerStartTime maybeNewTimer PastTimerLevel) timerLevels.previous
                                , Maybe.Extra.toList <|
                                    Maybe.map
                                        (editTimerLevel now timerStatus.timerStartTime maybeNewTimer CurrentTimerLevel)
                                        timerLevels.current
                                , List.map (editTimerLevel now timerStatus.timerStartTime maybeNewTimer FutureTimerLevel) timerLevels.future
                                ]
                            )
                        ]
                    , wrappedRow
                        [ width fill
                        , spacing 8
                        ]
                        [ -- TODO: switch to pdtab here?
                          controlsButton Theme.scheme5 (InputUpdateBlindOverlay shortGameEditBlindsSettings) <|
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
                        , controlsButton Theme.scheme5 (InputUpdateBlindOverlay mediumGameEditBlindsSettings) <|
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
                        , controlsButton Theme.scheme5 (InputUpdateBlindOverlay longGameEditBlindsSettings) <|
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
                    ]

            DoNotEditBlinds ->
                -- should close overlay?
                Element.none

            DoNotTrackBlinds ->
                -- unsupported
                Element.none
        , text ""
        ]


adminOverlay : Element Msg
adminOverlay =
    -- TODO: only show abandon round button when a round is active?
    --       (i.e. not on results screens)
    column
        [ width fill
        , spacing 10
        ]
        [ column
            [ width fill
            ]
            [ row
                [ width fill
                , spacing 8
                ]
                [ column
                    [ width fill
                    , spacing 8
                    ]
                    [ paragraph
                        [ width fill
                        , Font.alignRight
                        ]
                        [ text "abandon and restart the current round" ]
                    , paragraph
                        [ width fill
                        , Font.alignRight
                        , Font.size 15
                        ]
                        [ text "returns all bets, resets the dealer, and deals new cards" ]
                    ]
                , el
                    [ alignTop
                    , alignRight
                    ]
                  <|
                    controlsButton Theme.scheme1 AbandonRound <|
                        column
                            [ width fill
                            ]
                            [ el [ centerX ] <| text "restart"
                            , el [ centerX ] <| text "round"
                            ]
                ]
            ]
        ]


editTimerLevel : Posix -> Posix -> Maybe TimerStatus -> TimerChronology -> TimerLevelInfo -> Element Msg
editTimerLevel now timerStartTime maybeNewTimer timerChronology timerLevelInfo =
    let
        durationEl duration =
            let
                timeComponents =
                    millisToTimeComponents (duration * 1000)
            in
            row
                [ width fill
                , spacing 2
                ]
                [ formatTimeComponent timeComponents.minutes "m"
                , formatTimeComponent timeComponents.seconds "s"
                ]

        selectIcon =
            case ( timerChronology, maybeNewTimer ) of
                ( CurrentTimerLevel, Nothing ) ->
                    FontAwesome.Solid.redoAlt

                ( CurrentTimerLevel, Just _ ) ->
                    FontAwesome.Solid.play

                _ ->
                    FontAwesome.Solid.play
    in
    case timerLevelInfo.level of
        RoundLevel duration smallBlind ->
            row
                [ width fill
                , spacing 12
                , padding 6
                , Background.color
                    (case ( timerChronology, maybeNewTimer ) of
                        ( _, Just _ ) ->
                            Theme.colours.primary

                        ( PastTimerLevel, Nothing ) ->
                            Theme.colours.disabled

                        ( CurrentTimerLevel, Nothing ) ->
                            Theme.colours.highlightPrimary

                        ( FutureTimerLevel, Nothing ) ->
                            Theme.colours.primary
                    )
                , Font.color <| Theme.textColour Theme.colours.black
                ]
                [ Input.button
                    [ padding 4
                    , Border.rounded 2
                    , Border.solid
                    , Border.width 2
                    , Border.color Theme.colours.black
                    , Border.shadow
                        { offset = ( 5, 5 )
                        , size = 0
                        , blur = 0
                        , color = Theme.glow Theme.colours.secondary
                        }
                    , Font.size 25
                    , Font.color <| Theme.textColour Theme.colours.white
                    , Background.color Theme.colours.secondary
                    , focused
                        [ Background.color <| Theme.focusColour Theme.colours.secondary
                        , Border.color Theme.colours.white
                        , Border.shadow
                            { offset = ( 5, 5 )
                            , size = 0
                            , blur = 0
                            , color = Theme.glow <| Theme.focusColour Theme.colours.secondary
                            }
                        ]
                    , mouseOver
                        [ Background.color <| Theme.focusColour Theme.colours.secondary ]
                    ]
                    { onPress =
                        case maybeNewTimer of
                            Just newTimer ->
                                -- this is a new timer, so we need to set the levels and the progress at the same time
                                let
                                    newTimerStartTimeMs =
                                        posixToMillis now - (timerLevelInfo.startTimeOffset * 1000)
                                in
                                Just (UpdateBlindOverlay <| TimerBlinds { newTimer | timerStartTime = millisToPosix newTimerStartTimeMs })

                            Nothing ->
                                -- just change the progress on an existing timer, using the bespoke msg
                                Just (UpdateTimerProgressOverlay timerLevelInfo.startTimeOffset)
                    , label =
                        html <|
                            (selectIcon
                                |> FontAwesome.Icon.present
                                |> FontAwesome.Icon.styled [ FontAwesome.Attributes.xs ]
                                |> FontAwesome.Icon.withId ("restart-timer-level-" ++ String.fromInt smallBlind ++ "_pokerdot")
                                |> FontAwesome.Icon.titled ("Restart timer at " ++ String.fromInt smallBlind ++ " / " ++ String.fromInt (2 * smallBlind))
                                |> FontAwesome.Icon.view
                            )
                    }
                , el
                    [ width <| fillPortion 4 ]
                  <|
                    durationEl duration
                , el
                    []
                  <|
                    text <|
                        formatBlinds smallBlind
                ]

        BreakLevel duration ->
            row
                [ width fill ]
                [ text "break"
                , durationEl duration
                ]


type alias TimerLevelInfo =
    { level : TimerLevel
    , startTimeOffset : Int
    }


type alias TimerLevels =
    { current : Maybe TimerLevelInfo
    , previous : List TimerLevelInfo
    , future : List TimerLevelInfo
    }


type TimerChronology
    = PastTimerLevel
    | CurrentTimerLevel
    | FutureTimerLevel


timerLevelsFromStatus : Posix -> TimerStatus -> TimerLevels
timerLevelsFromStatus now timerStatus =
    let
        nowMillis =
            posixToMillis now

        startedMillis =
            posixToMillis timerStatus.timerStartTime

        timerProgress =
            case timerStatus.pausedTime of
                Just pausedTime ->
                    ceiling <| toFloat (posixToMillis pausedTime - startedMillis) / 1000

                Nothing ->
                    ceiling <| toFloat (nowMillis - startedMillis) / 1000

        ( _, accumulatedTimerLevels ) =
            List.foldl
                (\timerLevel ( progress, acc ) ->
                    let
                        newProgress =
                            case timerLevel of
                                RoundLevel duration _ ->
                                    progress + duration

                                BreakLevel duration ->
                                    progress + duration

                        timerLevelInfo =
                            { level = timerLevel, startTimeOffset = progress }
                    in
                    if newProgress < timerProgress then
                        -- this level finished before the current time, so previous level
                        ( newProgress
                        , { acc | previous = timerLevelInfo :: acc.previous }
                        )

                    else if progress > timerProgress then
                        -- we had not reached this level at the current time, must be future
                        ( newProgress
                        , { acc | future = timerLevelInfo :: acc.future }
                        )

                    else
                        -- must be current level if it isn't future or past
                        ( newProgress
                        , { acc | current = Just timerLevelInfo }
                        )
                )
                ( 0
                  -- elapsed timer after this timer level
                , { current = Nothing
                  , previous = []
                  , future = []
                  }
                )
                timerStatus.levels
    in
    { current = accumulatedTimerLevels.current
    , previous = List.reverse accumulatedTimerLevels.previous
    , future = List.reverse accumulatedTimerLevels.future
    }
