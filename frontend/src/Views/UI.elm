module Views.UI exposing (view)

import Browser
import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (labelLeft)
import Element.Region as Region
import FontAwesome.Attributes as Icon
import FontAwesome.Icon as Icon exposing (Icon)
import FontAwesome.Solid as Icon
import FontAwesome.Styles
import Model exposing (ActSelection, ChipsSettings(..), Game, Model, Msg(..), Player, PlayerId, Self, TimerLevel, TimerStatus, UI(..), Welcome, getGameCode)
import Views.Elements exposing (dotContainer, pdButton, pdButtonSmall, pdText, zWidths)


type alias Page =
    { body : Element Msg
    , title : String
    }


view : Model -> Browser.Document Msg
view model =
    let
        page =
            case model.ui of
                WelcomeScreen ->
                    { body = welcomeScreen model
                    , title = "PokerDot"
                    }

                HelpScreen ->
                    { body = helpScreen model
                    , title = "Help"
                    }

                CreateGameScreen gameName screenName ->
                    { body = createGameScreen model gameName screenName
                    , title = "New game"
                    }

                JoinGameScreen external gameCode screenName ->
                    { body = joinGameScreen model external gameCode screenName
                    , title = "Join game"
                    }

                LobbyScreen players chipsSettings self game welcome ->
                    { body = lobbyScreen model players chipsSettings self game welcome
                    , title = welcome.gameName ++ " | Waiting..."
                    }

                RejoinScreen welcome ->
                    { body = rejoinScreen model welcome
                    , title = welcome.gameName ++ " | Waiting..."
                    }

                WaitingGameScreen activePlayer self game welcome ->
                    { body = waitingGameScreen model activePlayer self game welcome
                    , title = welcome.gameName ++ " | Waiting..."
                    }

                ActingGameScreen currentAct self game welcome ->
                    { body = actingGameScreen model currentAct self game welcome
                    , title = welcome.gameName ++ " | Your turn"
                    }

                IdleGameScreen self game welcome ->
                    { body = idleGameScreen model self game welcome
                    , title = welcome.gameName ++ " | Your turn"
                    }

                CommunityCardsScreen game welcome ->
                    { body = communityCardsScreen model game welcome
                    , title = welcome.gameName
                    }

                TimerScreen timerStatus game welcome ->
                    { body = timerScreen model timerStatus game welcome
                    , title = welcome.gameName
                    }

                ChipSummaryScreen game welcome ->
                    { body = chipSummaryScreen model game welcome
                    , title = welcome.gameName
                    }
    in
    { title = page.title
    , body =
        [ layout
            [ Font.family
                [ Font.typeface "Nunito"
                , Font.sansSerif
                ]
            , Background.color <| rgb255 191 189 193
            ]
          <|
            column
                [ height fill
                , width fill
                ]
                [ -- header
                  row
                    [ width fill
                    ]
                    [ case model.ui of
                        WelcomeScreen ->
                            Element.none

                        _ ->
                            pdButtonSmall NavigateHome [ "home" ]
                    , el
                        [ alignRight
                        ]
                      <|
                        dotContainer model.viewport 80 <|
                            el
                                [ centerY
                                , centerX
                                ]
                            <|
                                if model.connected then
                                    text "1"

                                else
                                    text "0"
                    ]
                , if List.isEmpty model.errors then
                    Element.none

                  else
                    column
                        [ width fill ]
                    <|
                        List.map
                            (\error ->
                                Element.text error.failure.message
                            )
                            model.errors

                -- body
                , page.body
                ]
        ]
    }


welcomeScreen : Model -> Element Msg
welcomeScreen model =
    let
        radius =
            max 300 <|
                min 400 <|
                    round model.viewport.viewport.width
                        - 20
    in
    dotContainer model.viewport radius <|
        column
            [ width fill
            , height fill
            , centerX
            , spacing 48
            ]
            [ el
                [ centerX
                , paddingEach { zWidths | top = 100 }
                ]
              <|
                text "PokerDot"
            , row
                [ spacing 24
                , centerX
                ]
                [ pdButton NavigateCreateGame [ "Create", "game" ]
                , pdButton NavigateJoinGame [ "Join", "game" ]
                , row [] <|
                    List.map
                        (\welcomeMessage ->
                            row []
                                [ pdButton
                                    (NavigateGame welcomeMessage)
                                    [ welcomeMessage.screenName ++ " in " ++ welcomeMessage.gameName
                                    ]
                                , pdButton
                                    (DeletePersistedGame welcomeMessage)
                                    [ "x" ]
                                ]
                        )
                        model.library
                ]
            ]


helpScreen : Model -> Element Msg
helpScreen model =
    Element.none


createGameScreen : Model -> String -> String -> Element Msg
createGameScreen model gameName screenName =
    let
        radius =
            max 550 <|
                min 400 <|
                    round model.viewport.viewport.width
                        - 20

        tmp =
            minimum (round model.viewport.viewport.x - 100) <| px 400
    in
    dotContainer model.viewport radius <|
        column
            [ width fill
            , spacing 16
            , width <| maximum (round model.viewport.viewport.width - 24) <| px 400
            , paddingEach { zWidths | top = 150 }
            , centerX
            ]
            [ pdText (\newGameName -> InputCreateGame newGameName screenName) gameName "Game name"
            , pdText (InputCreateGame gameName) screenName "Your name"
            , el
                [ alignRight
                , paddingEach { zWidths | top = 24 }
                ]
              <|
                pdButton (SubmitCreateGame gameName screenName) [ "Create", "game" ]
            ]


joinGameScreen : Model -> Bool -> String -> String -> Element Msg
joinGameScreen model isExternal gameCode screenName =
    column
        [ width fill
        , spacing 16
        ]
        [ if isExternal then
            none

          else
            pdText (\newGameCode -> InputJoinGame isExternal newGameCode screenName) gameCode "Game code"
        , pdText (InputJoinGame isExternal gameCode) screenName "Your name"
        , pdButton (SubmitJoinGame gameCode screenName) [ "Join", "game" ]
        ]


lobbyScreen : Model -> List Player -> ChipsSettings -> Self -> Game -> Welcome -> Element Msg
lobbyScreen model playerOrder chipsSettings self game welcome =
    column
        [ width fill
        ]
        [ Element.text <| game.gameCode
        , if self.isAdmin then
            lobbyStartSettings playerOrder chipsSettings

          else
            none
        ]


lobbyStartSettings : List Player -> ChipsSettings -> Element Msg
lobbyStartSettings playerOrder chipsSettings =
    column
        [ width fill ]
        [ row
            [ spacing 8 ]
            [ text "No chips"
            , text "Timer"
            , text "Manual blinds"
            ]
        , case chipsSettings of
            DoNotTrackChips ->
                text "Not tracking chips"

            TrackWithTimer currentStackSizze timerLevels ->
                column
                    []
                    [ text "Timer levels"
                    , Input.text
                        []
                        { text = String.fromInt currentStackSizze
                        , label = Input.labelLeft [] <| text "Player stacks"
                        , placeholder =
                            Just <| Input.placeholder [] <| text "Player stacks"
                        , onChange =
                            \value ->
                                let
                                    stackSize =
                                        1
                                in
                                InputStartGameSettings playerOrder <|
                                    TrackWithTimer stackSize timerLevels
                        }
                    ]

            TrackWithManualBlinds stacks initialSmallBlind ->
                column
                    []
                    [ text "Manual blinds"
                    ]
        ]


rejoinScreen : Model -> Welcome -> Element Msg
rejoinScreen model welcome =
    Element.text <|
        "Hello again, "
            ++ welcome.screenName
            ++ ". Rejoining "
            ++ welcome.gameName
            ++ "."


waitingGameScreen : Model -> PlayerId -> Self -> Game -> Welcome -> Element Msg
waitingGameScreen model activePlayer self game welcome =
    Element.none


actingGameScreen : Model -> ActSelection -> Self -> Game -> Welcome -> Element Msg
actingGameScreen model currentAct self game welcome =
    Element.none


idleGameScreen : Model -> Self -> Game -> Welcome -> Element Msg
idleGameScreen model self game welcome =
    Element.none


communityCardsScreen : Model -> Game -> Welcome -> Element Msg
communityCardsScreen model game welcome =
    Element.none


timerScreen : Model -> TimerStatus -> Game -> Welcome -> Element Msg
timerScreen model timerStatus game welcome =
    Element.none


chipSummaryScreen : Model -> Game -> Welcome -> Element Msg
chipSummaryScreen model game welcome =
    Element.none
