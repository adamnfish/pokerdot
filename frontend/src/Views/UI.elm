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
import Model exposing (ActSelection(..), Card, ChipsSettings(..), Game, Hand(..), Model, Msg(..), Player, PlayerId, PlayerWinnings, PlayingState(..), PotResult, Self, TimerLevel, TimerStatus, UI(..), Welcome)
import Views.Elements exposing (dotContainer, pdButton, pdButtonSmall, pdTab, pdText, zWidths)


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
        [ selfUi model.peeking self
        , tableUi game
        , communityCardsScreen model game welcome
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
        [ selfUi model.peeking self
        , tableUi game
        , column
            []
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
                        []
                        [ text name
                        , Maybe.withDefault none <| Maybe.map handUi pw.hand
                        , text <| "Winnings: " ++ String.fromInt pw.winnings
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
        [ selfUi model.peeking self
        , tableUi game
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


communityCardsScreen : Model -> Game -> Welcome -> Element Msg
communityCardsScreen model game welcome =
    row
        [ width fill ]
    <|
        case game.round of
            Model.PreFlopRound ->
                []

            Model.FlopRound flop1 flop2 flop3 ->
                List.map cardUi [ flop1, flop2, flop3 ]

            Model.TurnRound flop1 flop2 flop3 turn ->
                List.map cardUi [ flop1, flop2, flop3, turn ]

            Model.RiverRound flop1 flop2 flop3 turn river ->
                List.map cardUi [ flop1, flop2, flop3, turn, river ]

            Model.ShowdownRound flop1 flop2 flop3 turn river _ ->
                List.map cardUi [ flop1, flop2, flop3, turn, river ]


timerScreen : Model -> TimerStatus -> Game -> Welcome -> Element Msg
timerScreen model timerStatus game welcome =
    Element.none


chipSummaryScreen : Model -> Game -> Welcome -> Element Msg
chipSummaryScreen model game welcome =
    Element.none


tableUi : Game -> Element Msg
tableUi game =
    let
        pot =
            List.sum <| List.map .pot game.players

        seat : Player -> Element Msg
        seat player =
            row
                [ width fill
                , spacing 8
                ]
                [ text player.screenName
                , text <| String.fromInt player.stack
                , text <| String.fromInt player.bet
                ]
    in
    column
        [ width fill
        ]
        [ column
            [ width fill ]
          <|
            List.map seat game.players
        , text <| "pot: " ++ String.fromInt pot
        ]


selfUi : Bool -> Self -> Element Msg
selfUi isPeeking self =
    if self.busted then
        row
            [ width fill ]
            [ text self.screenName ]

    else
        row
            [ width fill ]
            [ text self.screenName
            , text " "
            , case self.hole of
                Nothing ->
                    text " - "

                Just ( card1, card2 ) ->
                    row
                        []
                    <|
                        if isPeeking then
                            List.intersperse (text " ") <| List.map cardUi [ card1, card2 ]

                        else
                            [ text " - ", text " - " ]
            , pdTab TogglePeek <|
                if isPeeking then
                    "stop looking at hand"

                else
                    "look at hand"
            ]


cardUi : Card -> Element Msg
cardUi card =
    let
        rank =
            case card.rank of
                Model.Two ->
                    text "2"

                Model.Three ->
                    text "3"

                Model.Four ->
                    text "4"

                Model.Five ->
                    text "5"

                Model.Six ->
                    text "6"

                Model.Seven ->
                    text "7"

                Model.Eight ->
                    text "8"

                Model.Nine ->
                    text "9"

                Model.Ten ->
                    text "10"

                Model.Jack ->
                    text "J"

                Model.Queen ->
                    text "Q"

                Model.King ->
                    text "K"

                Model.Ace ->
                    text "A"

        suit =
            case card.suit of
                Model.Clubs ->
                    text "♣"

                Model.Diamonds ->
                    text "♦"

                Model.Spades ->
                    text "♠"

                Model.Hearts ->
                    text "♥"
    in
    row [] [ rank, suit ]


handUi : Hand -> Element Msg
handUi hand =
    case hand of
        HighCard c1 c2 c3 c4 c5 ->
            column
                []
                [ text "High card"
                , row
                    []
                    [ cardUi c1
                    , cardUi c2
                    , cardUi c3
                    , cardUi c4
                    , cardUi c5
                    ]
                ]

        Pair p1 p2 k1 k2 k3 ->
            column
                []
                [ text "Pair"
                , row
                    []
                    [ cardUi p1
                    , cardUi p2
                    , text " | "
                    , cardUi k1
                    , cardUi k2
                    , cardUi k3
                    ]
                ]

        TwoPair p11 p12 p21 p22 k ->
            column
                []
                [ text "Two pair"
                , row
                    []
                    [ cardUi p11
                    , cardUi p12
                    , text " | "
                    , cardUi p21
                    , cardUi p22
                    , text " | "
                    , cardUi k
                    ]
                ]

        ThreeOfAKind t1 t2 t3 k1 k2 ->
            column
                []
                [ text "Three of a kind"
                , row
                    []
                    [ cardUi t1
                    , cardUi t2
                    , cardUi t3
                    , text " | "
                    , cardUi k1
                    , cardUi k2
                    ]
                ]

        Straight c1 c2 c3 c4 c5 ->
            column
                []
                [ text "Straight"
                , row
                    []
                    [ cardUi c1
                    , cardUi c2
                    , cardUi c3
                    , cardUi c4
                    , cardUi c5
                    ]
                ]

        Flush c1 c2 c3 c4 c5 ->
            column
                []
                [ text "Flush"
                , row
                    []
                    [ cardUi c1
                    , cardUi c2
                    , cardUi c3
                    , cardUi c4
                    , cardUi c5
                    ]
                ]

        FullHouse t1 t2 t3 p1 p2 ->
            column
                []
                [ text "Full house"
                , row
                    []
                    [ cardUi t1
                    , cardUi t2
                    , cardUi t3
                    , text " | "
                    , cardUi p1
                    , cardUi p2
                    ]
                ]

        FourOfAKind q1 q2 q3 q4 k ->
            column
                []
                [ text "Four of a kind"
                , row
                    []
                    [ cardUi q1
                    , cardUi q2
                    , cardUi q3
                    , cardUi q4
                    , text " | "
                    , cardUi k
                    ]
                ]

        StraightFlush c1 c2 c3 c4 c5 ->
            column
                []
                [ text "Straight flush"
                , row
                    []
                    [ cardUi c1
                    , cardUi c2
                    , cardUi c3
                    , cardUi c4
                    , cardUi c5
                    ]
                ]
