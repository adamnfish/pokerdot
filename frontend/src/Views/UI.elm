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
import Model exposing (ActSelection, Game, Model, Msg(..), Player, PlayerId, Self, TimerStatus, UI(..), Welcome)


view : Model -> Browser.Document Msg
view model =
    let
        body =
            case model.ui of
                WelcomeScreen ->
                    welcomeScreen model

                HelpScreen ->
                    helpScreen model

                CreateGameScreen gameName screenName ->
                    createGameScreen model gameName screenName

                JoinGameScreen gameCode screenName ->
                    joinGameScreen model gameCode screenName

                LobbyScreen players maybeGameStatus welcome ->
                    lobbyScreen model players maybeGameStatus welcome

                RejoinScreen welcome ->
                    rejoinScreen model welcome

                WaitingGameScreen activePlayer self game welcome ->
                    waitingGameScreen model activePlayer self game welcome

                ActingGameScreen currentAct self game welcome ->
                    actingGameScreen model currentAct self game welcome

                CommunityCardsScreen game welcome ->
                    communityCardsScreen model game welcome

                TimerScreen timerStatus game welcome ->
                    timerScreen model timerStatus game welcome

                ChipSummaryScreen game welcome ->
                    chipSummaryScreen model game welcome
    in
    { title = "pokerdot"
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
                    [ width fill ]
                    [ Input.button
                        [ Border.rounded 40
                        , width <| px 80
                        , height <| px 80
                        , Border.solid
                        , Border.width 2
                        , Border.color <| rgb255 60 60 60
                        ]
                        { onPress = Just NavigateHome
                        , label = text "home"
                        }
                    ]

                -- body
                , body

                -- footer
                , row
                    [ width fill ]
                    [ text "footer" ]
                ]
        ]
    }


welcomeScreen : Model -> Element Msg
welcomeScreen model =
    row
        [ width fill
        , spacing 8
        ]
        [ Input.button
            [ Border.rounded 50
            , width <| px 100
            , height <| px 100
            , Border.solid
            , Border.width 2
            , Border.color <| rgb255 60 60 60
            ]
            { onPress = Just NavigateCreateGame
            , label =
                paragraph []
                    [ text "Create "
                    , text " game"
                    ]
            }
        , Input.button
            [ Border.rounded 50
            , width <| px 100
            , height <| px 100
            , Border.solid
            , Border.width 2
            , Border.color <| rgb255 60 60 60
            ]
            { onPress = Just NavigateJoinGame
            , label =
                paragraph []
                    [ text "Join "
                    , text " game"
                    ]
            }
        ]


helpScreen : Model -> Element Msg
helpScreen model =
    Element.none


createGameScreen : Model -> String -> String -> Element Msg
createGameScreen model gameName screenName =
    column
        [ width fill
        , spacing 8
        ]
        [ Input.text
            [ Font.alignLeft
            , paddingXY 10 8
            , Border.solid
            , Border.color <| rgb255 60 60 60
            , Border.widthEach { zWidths | bottom = 2 }
            , Border.rounded 0
            , Background.color <| rgb255 200 200 200
            ]
            { onChange = \newGameName -> InputCreateGame newGameName screenName
            , text = gameName
            , placeholder = Nothing
            , label = labelLeft [] <| text "Game name"
            }
        , Input.text
            [ Font.alignLeft
            , paddingXY 10 8
            , Border.solid
            , Border.width 2
            , Border.color <| rgb255 60 60 60
            , Border.widthEach { zWidths | bottom = 2 }
            , Border.rounded 0
            , Background.color <| rgb255 200 200 200
            ]
            { onChange = InputCreateGame gameName
            , text = screenName
            , placeholder = Nothing
            , label = labelLeft [] <| text "Your name"
            }
        , Input.button
            [ Border.rounded 50
            , width <| px 100
            , height <| px 100
            , Border.solid
            , Border.width 2
            , Border.color <| rgb255 60 60 60
            ]
            { onPress = Just <| SubmitCreateGame gameName screenName
            , label =
                paragraph []
                    [ text "Create "
                    , text " game"
                    ]
            }
        ]


joinGameScreen : Model -> String -> String -> Element Msg
joinGameScreen model gameCode screenName =
    Element.none


lobbyScreen : Model -> List Player -> Maybe ( Self, Game ) -> Welcome -> Element Msg
lobbyScreen model players maybeGameDetails welcome =
    Element.none


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


zWidths : { bottom : Int, left : Int, right : Int, top : Int }
zWidths =
    { bottom = 0
    , left = 0
    , right = 0
    , top = 0
    }
