module Views.UI exposing (view)

import Browser
import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (labelLeft)
import Element.Region as Region
import FontAwesome.Attributes
import FontAwesome.Icon
import FontAwesome.Solid
import FontAwesome.Styles
import List.Extra
import Model exposing (ActSelection(..), Card, ChipsSettings(..), Game, Hand(..), Model, Msg(..), Player, PlayerId, PlayerWinnings, PlayingState(..), PotResult, Round(..), Self, TimerLevel, TimerStatus, UI(..), Welcome)
import Views.Elements exposing (cardUi, communityCardsUi, connectionUi, controlsButton, dotContainer, handUi, pdButton, pdButtonSmall, pdTab, pdText, pokerControlsUi, selfUi, tableUi, uiElements, zWidths)
import Views.Generators exposing (cardsGen)


type alias Page =
    { body : Element Msg
    , title : String
    , header : Maybe (Element Msg)
    }


view : Model -> Browser.Document Msg
view model =
    let
        page =
            case model.ui of
                WelcomeScreen ->
                    { body = welcomeScreen model
                    , title = "PokerDot"
                    , header =
                        Just <| welcomeHeader model.connected
                    }

                HelpScreen ->
                    { body = helpScreen model
                    , title = "Help"
                    , header = Nothing
                    }

                CreateGameScreen gameName screenName ->
                    { body = createGameScreen model gameName screenName
                    , title = "New game"
                    , header = Nothing
                    }

                JoinGameScreen external gameCode screenName ->
                    { body = joinGameScreen model external gameCode screenName
                    , title = "Join game"
                    , header = Nothing
                    }

                LobbyScreen players chipsSettings self game welcome ->
                    { body = lobbyScreen model players chipsSettings self game welcome
                    , title = welcome.gameName ++ " | Waiting..."
                    , header = Nothing
                    }

                RejoinScreen welcome ->
                    { body = rejoinScreen model welcome
                    , title = welcome.gameName ++ " | Waiting..."
                    , header = Nothing
                    }

                GameScreen currentAct self game welcome ->
                    let
                        playingState =
                            case game.inTurn of
                                Just currentPlayerId ->
                                    if currentPlayerId == self.playerId then
                                        Playing

                                    else
                                        Waiting

                                Nothing ->
                                    Idle
                    in
                    { body = gameScreen model playingState currentAct self game welcome
                    , title =
                        welcome.gameName
                            ++ (case playingState of
                                    Playing ->
                                        " | Your turn"

                                    Waiting ->
                                        " | Waiting"

                                    Idle ->
                                        " | TODO"
                               )
                    , header = Nothing

                    -- TODO: what should this say
                    }

                RoundResultScreen potResults playerWinnings self game welcome ->
                    { body = roundResultsScreen model potResults playerWinnings self game welcome
                    , title = welcome.gameName ++ " | Round ended"
                    , header = Nothing
                    }

                GameResultScreen self game welcome ->
                    { body = gameResultsScreen model self game welcome
                    , title = welcome.gameName ++ " | Round ended"
                    , header = Nothing
                    }

                CommunityCardsScreen game welcome ->
                    { body = communityCardsUi game.round
                    , title = welcome.gameName
                    , header = Nothing
                    }

                TimerScreen timerStatus game welcome ->
                    { body = timerScreen model timerStatus game welcome
                    , title = welcome.gameName
                    , header = Nothing
                    }

                ChipSummaryScreen game welcome ->
                    { body = chipSummaryScreen model game welcome
                    , title = welcome.gameName
                    , header = Nothing
                    }

                UIElementsScreen seed act ->
                    { body = uiElements model seed act
                    , title = "Debugging | UI Elements"
                    , header = Nothing
                    }
    in
    { title = page.title
    , body =
        [ FontAwesome.Styles.css
        , layout
            [ Font.family
                [ Font.typeface "Nunito"
                , Font.sansSerif
                ]
            , Background.color <| rgb255 191 189 193
            , inFront <|
                if List.isEmpty model.errors then
                    Element.none

                else
                    column
                        [ width fill
                        , paddingXY 10 20
                        , spacing 10
                        , alignBottom
                        , Background.color <| rgb255 200 150 150
                        ]
                    <|
                        List.map
                            (\error -> paragraph [] [ text error.failure.message ])
                        <|
                            List.reverse model.errors
            ]
          <|
            column
                [ height fill
                , width fill
                ]
                [ case page.header of
                    Nothing ->
                        row
                            [ width fill
                            , Background.color <| rgb255 50 50 50
                            ]
                            [ case model.ui of
                                WelcomeScreen ->
                                    Element.none

                                _ ->
                                    Input.button
                                        [ paddingXY 10 15
                                        , Background.color <| rgb255 80 80 80
                                        , Font.size 25
                                        , Font.color <| rgb255 200 200 200
                                        ]
                                        { onPress = Just NavigateHome
                                        , label = text "home"
                                        }
                            , el
                                [ alignRight
                                , padding 15
                                , Font.color <| rgb255 200 200 200
                                ]
                              <|
                                connectionUi model.connected
                            ]

                    Just headerEl ->
                        headerEl

                -- body
                , page.body
                ]
        ]
    }


welcomeHeader : Bool -> Element Msg
welcomeHeader connected =
    column
        [ width fill
        , height <| px 200
        , Background.color <| rgb255 50 50 50
        , inFront <|
            el
                [ alignRight
                , padding 15
                , Font.color <| rgb255 200 200 200
                ]
            <|
                connectionUi connected
        ]
        [ el
            [ width fill
            , centerY
            , Font.center
            , Font.size 30
            , Font.color <| rgb255 200 200 200
            ]
          <|
            text "Pokerdot"
        ]


welcomeScreen : Model -> Element Msg
welcomeScreen model =
    let
        radius =
            max 300 <|
                min 400 <|
                    round model.viewport.viewport.width
                        - 20
    in
    column
        [ width fill
        , height fill
        , centerX
        , spacing 48
        , paddingXY 0 50
        ]
        [ row
            [ centerX
            , width fill
            ]
            [ el
                [ centerX ]
              <|
                controlsButton NavigateCreateGame <|
                    column
                        [ width fill
                        ]
                        [ el
                            [ width fill
                            , Font.center
                            ]
                          <|
                            text "Create"
                        , el
                            [ width fill
                            , Font.center
                            ]
                          <|
                            text "game"
                        ]
            , el [ width <| px 30 ] Element.none
            , el [ centerX ] <|
                controlsButton NavigateJoinGame <|
                    column
                        [ width fill
                        ]
                        [ el
                            [ width fill
                            , Font.center
                            ]
                          <|
                            text "Join"
                        , el
                            [ width fill
                            , Font.center
                            ]
                          <|
                            text "game"
                        ]
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
    column
        [ width fill
        , spacing 16
        , width <| maximum (round model.viewport.viewport.width - 24) <| px 500
        , paddingEach { zWidths | top = 50 }
        , centerX
        ]
        [ pdText (\newGameName -> InputCreateGame newGameName screenName) gameName "Game name"
        , pdText (InputCreateGame gameName) screenName "Your name"
        , el
            [ alignRight
            ]
          <|
            controlsButton (SubmitCreateGame gameName screenName) <|
                column
                    [ width fill ]
                    [ el
                        [ width fill
                        , Font.center
                        ]
                      <|
                        text "Create"
                    , el
                        [ width fill
                        , Font.center
                        ]
                      <|
                        text "game"
                    ]
        ]


joinGameScreen : Model -> Bool -> String -> String -> Element Msg
joinGameScreen model isExternal gameCode screenName =
    column
        [ width fill
        , spacing 16
        , width <| maximum (round model.viewport.viewport.width - 24) <| px 500
        , centerX
        , paddingEach { zWidths | top = 50 }
        ]
        [ if isExternal then
            none

          else
            pdText (\newGameCode -> InputJoinGame isExternal newGameCode screenName) gameCode "Game code"
        , pdText (InputJoinGame isExternal gameCode) screenName "Your name"
        , el
            [ alignRight ]
          <|
            controlsButton (SubmitJoinGame gameCode screenName) <|
                column
                    [ width fill ]
                    [ el
                        [ width fill
                        , Font.center
                        ]
                      <|
                        text "Join"
                    , el
                        [ width fill
                        , Font.center
                        ]
                      <|
                        text "game"
                    ]
        ]


lobbyScreen : Model -> List Player -> ChipsSettings -> Self -> Game -> Welcome -> Element Msg
lobbyScreen model playerOrder chipsSettings self game welcome =
    let
        formatPlayer : Player -> Element Msg
        formatPlayer player =
            text player.screenName
    in
    column
        [ width fill
        , width <| maximum (round model.viewport.viewport.width - 24) <| px 500
        , centerX
        , paddingEach { zWidths | top = 15 }
        , spacing 15
        ]
        [ row
            [ width fill
            , spacing 15
            ]
            [ el
                [ padding 15
                , alignLeft
                , Font.size 40
                , Font.alignLeft
                , Background.color <| rgb255 255 255 255
                , Border.solid
                , Border.color <| rgb255 0 0 0
                , Border.widthEach
                    { top = 1
                    , bottom = 3
                    , left = 1
                    , right = 1
                    }
                ]
              <|
                text <|
                    game.gameCode
            , column
                [ spacing 4 ]
                [ el
                    [ width fill
                    , Font.alignLeft
                    , Font.size 25
                    ]
                  <|
                    text "Game code"
                , link
                    []
                    { url = "/#join/" ++ game.gameCode
                    , label =
                        row
                            [ spacing 4 ]
                            [ html <|
                                (FontAwesome.Solid.shareAlt
                                    |> FontAwesome.Icon.present
                                    |> FontAwesome.Icon.styled [ FontAwesome.Attributes.sm ]
                                    |> FontAwesome.Icon.withId "share-join-link_pokerdot"
                                    |> FontAwesome.Icon.titled "Share join link"
                                    |> FontAwesome.Icon.view
                                )
                            , text " "
                            , text "Join link"
                            ]
                    }
                ]
            ]
        , row
            [ width fill ]
            []
        , column [] <|
            List.map formatPlayer playerOrder
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
            [ pdTab (InputStartGameSettings playerOrder DoNotTrackChips) "No chips"
            , pdTab (InputStartGameSettings playerOrder <| TrackWithTimer 1000 []) "Timer"
            , pdTab (InputStartGameSettings playerOrder <| TrackWithManualBlinds 1000 5) "Manual blinds"
            ]
        , case chipsSettings of
            DoNotTrackChips ->
                text "Not tracking chips"

            TrackWithTimer currentStackSize timerLevels ->
                column
                    [ width fill ]
                    [ text "Timer levels"
                    , Input.text
                        []
                        { text = String.fromInt currentStackSize
                        , label =
                            Input.labelLeft
                                [ width <| px 150
                                , Font.alignLeft
                                ]
                            <|
                                text "Player stacks"
                        , placeholder =
                            Just <|
                                Input.placeholder
                                    []
                                <|
                                    text "Player stacks"
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

            TrackWithManualBlinds currentStackSize initialSmallBlind ->
                column
                    [ width fill ]
                    [ Input.text
                        []
                        { text = String.fromInt currentStackSize
                        , label =
                            Input.labelLeft
                                [ width <| px 150
                                , Font.alignLeft
                                ]
                            <|
                                text "Player stacks"
                        , placeholder =
                            Just <| Input.placeholder [] <| text "Player stacks"
                        , onChange =
                            \value ->
                                let
                                    stackSize =
                                        Maybe.withDefault 0 <| String.toInt value
                                in
                                InputStartGameSettings playerOrder <|
                                    TrackWithManualBlinds stackSize initialSmallBlind
                        }
                    , Input.text
                        []
                        { text = String.fromInt initialSmallBlind
                        , label =
                            Input.labelLeft
                                [ width <| px 150
                                , Font.alignLeft
                                ]
                            <|
                                text "Small blind"
                        , placeholder =
                            Just <| Input.placeholder [] <| text "Initial small blind"
                        , onChange =
                            \value ->
                                let
                                    smallBlind =
                                        Maybe.withDefault 0 <| String.toInt value
                                in
                                InputStartGameSettings playerOrder <|
                                    TrackWithManualBlinds currentStackSize smallBlind
                        }
                    ]
        , row
            []
            [ pdButton SubmitStartGame [ "Start" ]
            ]
        ]


rejoinScreen : Model -> Welcome -> Element Msg
rejoinScreen model welcome =
    column
        [ width fill
        , width <| maximum (round model.viewport.viewport.width - 24) <| px 500
        , centerX
        , paddingEach { zWidths | top = 15 }
        , spacing 15
        ]
        [ Element.text <|
            "Hello again, "
                ++ welcome.screenName
                ++ ". Rejoining "
                ++ welcome.gameName
                ++ "."
        ]


gameScreen : Model -> PlayingState -> ActSelection -> Self -> Game -> Welcome -> Element Msg
gameScreen model playingState currentAct self game welcome =
    column
        [ width fill
        , width <| maximum (round model.viewport.viewport.width - 24) <| px 500
        , centerX
        , paddingEach { zWidths | top = 15 }
        , spacing 15
        ]
        [ tableUi game.round game.players
        , selfUi model.peeking self
        , case playingState of
            Playing ->
                pokerControlsUi True currentAct self game.players

            Waiting ->
                pokerControlsUi False currentAct self game.players

            Idle ->
                if self.isAdmin then
                    el
                        [ alignRight ]
                    <|
                        controlsButton AdvancePhase <|
                            column
                                [ width fill ]
                                [ el
                                    [ width fill
                                    , Font.center
                                    ]
                                  <|
                                    text "Next"
                                , el
                                    [ width fill
                                    , Font.center
                                    ]
                                  <|
                                    text "round"
                                ]

                else
                    text "Waiting for the next round to start"
        ]


roundResultsScreen : Model -> List PotResult -> List PlayerWinnings -> Self -> Game -> Welcome -> Element Msg
roundResultsScreen model potResults playerWinnings self game welcome =
    column
        [ width fill
        , width <| maximum (round model.viewport.viewport.width - 24) <| px 500
        , centerX
        , paddingEach { zWidths | top = 15 }
        , spacing 15
        ]
        [ tableUi game.round game.players
        , selfUi model.peeking self
        , column
            [ width fill
            , spacing 5
            ]
          <|
            List.map
                (\pw ->
                    let
                        name =
                            Maybe.withDefault "player" <|
                                Maybe.map .screenName <|
                                    List.Extra.find (\p -> p.playerId == pw.playerId) game.players
                    in
                    column
                        [ width fill
                        , spacing 5
                        ]
                        [ Maybe.withDefault none <| Maybe.map (handUi name pw.winnings (Just pw.hole)) pw.hand ]
                )
                playerWinnings
        , if self.isAdmin then
            el
                [ alignRight ]
            <|
                controlsButton AdvancePhase <|
                    column
                        [ width fill ]
                        [ el
                            [ width fill
                            , Font.center
                            ]
                          <|
                            text "Next"
                        , el
                            [ width fill
                            , Font.center
                            ]
                          <|
                            text "round"
                        ]

          else
            text "Waiting for the next round to start"
        ]


gameResultsScreen : Model -> Self -> Game -> Welcome -> Element Msg
gameResultsScreen model self game welcome =
    column
        [ width fill
        , width <| maximum (round model.viewport.viewport.width - 24) <| px 500
        , centerX
        , paddingEach { zWidths | top = 15 }
        , spacing 15
        ]
        [ tableUi game.round game.players
        , selfUi model.peeking self
        ]


timerScreen : Model -> TimerStatus -> Game -> Welcome -> Element Msg
timerScreen model timerStatus game welcome =
    Element.none


chipSummaryScreen : Model -> Game -> Welcome -> Element Msg
chipSummaryScreen model game welcome =
    Element.none
