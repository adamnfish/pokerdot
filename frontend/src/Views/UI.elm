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
import List.Extra
import Model exposing (ActSelection(..), Card, ChipsSettings(..), Game, Hand(..), Model, Msg(..), Player, PlayerId, PlayerWinnings, PlayingState(..), PotResult, Round(..), Self, TimerLevel, TimerStatus, UI(..), Welcome)
import Views.Elements exposing (cardUi, communityCardsUi, dotContainer, handUi, pdButton, pdButtonSmall, pdTab, pdText, selfUi, tableUi, uiElements, zWidths)
import Views.Generators exposing (cardsGen)


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

                    -- TODO: what should this say
                    }

                RoundResultScreen potResults playerWinnings self game welcome ->
                    { body = roundResultsScreen model potResults playerWinnings self game welcome
                    , title = welcome.gameName ++ " | Round ended"
                    }

                GameResultScreen self game welcome ->
                    { body = gameResultsScreen model self game welcome
                    , title = welcome.gameName ++ " | Round ended"
                    }

                CommunityCardsScreen game welcome ->
                    { body = communityCardsUi game.round
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

                UIElementsScreen seed ->
                    { body = uiElements seed
                    , title = "Debugging | UI Elements"
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
    let
        formatPlayer : Player -> Element Msg
        formatPlayer player =
            text player.screenName
    in
    column
        [ width fill
        ]
        [ Element.text <| game.gameCode
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

            TrackWithManualBlinds currentStackSize initialSmallBlind ->
                column
                    [ width fill ]
                    [ text "Manual blinds"
                    , Input.text
                        []
                        { text = String.fromInt currentStackSize
                        , label = Input.labelLeft [] <| text "Player stacks"
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
                        , label = Input.labelLeft [] <| text "Small blind"
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
    Element.text <|
        "Hello again, "
            ++ welcome.screenName
            ++ ". Rejoining "
            ++ welcome.gameName
            ++ "."


gameScreen : Model -> PlayingState -> ActSelection -> Self -> Game -> Welcome -> Element Msg
gameScreen model playingState currentAct self game welcome =
    column
        [ width fill
        ]
        [ tableUi game.round game.players
        , selfUi model.peeking self
        , case playingState of
            Playing ->
                pokerControlsScreen True currentAct self game

            Waiting ->
                pokerControlsScreen False currentAct self game

            Idle ->
                if self.isAdmin then
                    pdButton AdvancePhase [ "Next round" ]

                else
                    text "Waiting for the next round to start"
        ]


roundResultsScreen : Model -> List PotResult -> List PlayerWinnings -> Self -> Game -> Welcome -> Element Msg
roundResultsScreen model potResults playerWinnings self game welcome =
    column
        [ width fill
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

                        maybeHole =
                            Maybe.andThen .hole <|
                                List.Extra.find (\p -> p.playerId == pw.playerId) game.players
                    in
                    column
                        [ width fill
                        , spacing 5
                        ]
                        [ Maybe.withDefault none <| Maybe.map (handUi name pw.winnings maybeHole) pw.hand

                        -- TODO: the above doesn't work! better to wire it through the PlayerWinnings type
                        ]
                )
                playerWinnings
        , if self.isAdmin then
            pdButton AdvancePhase [ "Next round" ]

          else
            text "Waiting for the next round to start"
        ]


gameResultsScreen : Model -> Self -> Game -> Welcome -> Element Msg
gameResultsScreen model self game welcome =
    column
        [ width fill
        ]
        [ tableUi game.round game.players
        , selfUi model.peeking self
        ]


pokerControlsScreen : Bool -> ActSelection -> Self -> Game -> Element Msg
pokerControlsScreen isActive actSelection self game =
    let
        playerBets =
            List.map .bet game.players

        maxBet =
            Maybe.withDefault 0 <| List.maximum playerBets

        -- TODO: this is not yet correct
        --       make sure it takes into account all in calls and the minimum bet amount
        callAmount =
            maxBet - self.bet

        currentSelectedBetAmount =
            case actSelection of
                ActBet amount ->
                    amount

                _ ->
                    0
    in
    column
        []
        [ pdButtonSmall (InputActSelection ActCheck) [ "check" ]
        , pdButtonSmall (InputActSelection ActCall) [ "call" ]
        , pdButtonSmall (InputActSelection ActFold) [ "fold" ]
        , row
            []
            [ pdText
                (\str ->
                    InputActSelection <| ActBet <| Maybe.withDefault 0 <| String.toInt str
                )
                (String.fromInt currentSelectedBetAmount)
                "bet amount"
            , pdButtonSmall (InputActSelection <| ActBet currentSelectedBetAmount) [ "bet" ]
            ]
        , if isActive then
            case actSelection of
                ActCheck ->
                    pdButtonSmall Check [ "Confirm check" ]

                ActCall ->
                    pdButtonSmall (Bet callAmount) [ "Confirm call" ]

                ActFold ->
                    pdButtonSmall Fold [ "Confirm fold" ]

                ActBet amount ->
                    pdButtonSmall (Bet amount) [ "Confirm bet" ]

                NoAct ->
                    text "Select action"

          else
            text "It isn't your turn"
        ]


timerScreen : Model -> TimerStatus -> Game -> Welcome -> Element Msg
timerScreen model timerStatus game welcome =
    Element.none


chipSummaryScreen : Model -> Game -> Welcome -> Element Msg
chipSummaryScreen model game welcome =
    Element.none
