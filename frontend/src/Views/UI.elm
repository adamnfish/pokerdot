module Views.UI exposing (..)

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
import Model exposing (ActSelection, Game, Model, Msg(..), Player, PlayerId, Self, TimerStatus, UI(..), Welcome, getGameCode)
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

                JoinGameScreen gameCode screenName ->
                    { body = joinGameScreen model gameCode screenName
                    , title = "Join game"
                    }

                LobbyScreen players maybeGameStatus welcome ->
                    { body = lobbyScreen model players maybeGameStatus welcome
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


joinGameScreen : Model -> String -> String -> Element Msg
joinGameScreen model gameCode screenName =
    column
        [ width fill
        , spacing 16
        ]
        [ pdText (\newGameCode -> InputJoinGame newGameCode screenName) gameCode "Game code"
        , pdText (InputJoinGame gameCode) screenName "Your name"
        , pdButton (SubmitJoinGame gameCode screenName) [ "Join", "game" ]
        ]


lobbyScreen : Model -> List Player -> Maybe ( Self, Game ) -> Welcome -> Element Msg
lobbyScreen model players maybeGameDetails welcome =
    column
        [ width fill
        ]
        [ Element.text <| getGameCode welcome.gameId ]


rejoinScreen : Model -> Welcome -> Element Msg
rejoinScreen model welcome =
    Element.none


waitingGameScreen : Model -> PlayerId -> Self -> Game -> Welcome -> Element Msg
waitingGameScreen model activePlayer self game welcome =
    Element.none


actingGameScreen : Model -> ActSelection -> Self -> Game -> Welcome -> Element Msg
actingGameScreen model currentAct self game welcome =
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