module Views.UI exposing (view)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events
import Element.Font as Font
import Element.Input as Input
import FontAwesome.Attributes
import FontAwesome.Icon
import FontAwesome.Solid
import FontAwesome.Styles
import List.Extra
import Messages exposing (lookupPlayer)
import Model exposing (ActSelection(..), Action(..), Card, ChipsSettings(..), Game, Hand(..), LoadingStatus(..), Model, Msg(..), Player, PlayerId, PlayerWinnings, PlayingState(..), PotResult, Round(..), Self, TimerLevel, TimerStatus, UI(..), Welcome)
import Views.Elements exposing (communityCardsUi, connectionUi, container, controlsButton, handUi, pdTab, pdText, pokerControlsUi, selfUi, tableUi, uiElements, zWidths)
import Views.Theme as Theme


type alias Page =
    { body : Element Msg
    , title : String
    , header : Maybe (Element Msg)
    , scheme : Maybe Theme.Scheme
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
                    , scheme = Just Theme.scheme1
                    }

                HelpScreen ->
                    { body = helpScreen model
                    , title = "Help"
                    , header = Nothing
                    , scheme = Just Theme.scheme4
                    }

                CreateGameScreen gameName screenName ->
                    { body = createGameScreen model gameName screenName
                    , title = "New game"
                    , header = Nothing
                    , scheme = Just Theme.scheme3
                    }

                JoinGameScreen external gameCode screenName ->
                    { body = joinGameScreen model external gameCode screenName
                    , title = "Join game"
                    , header = Nothing
                    , scheme = Just Theme.scheme3
                    }

                LobbyScreen players chipsSettings self game welcome ->
                    { body = lobbyScreen model players chipsSettings self game welcome
                    , title = welcome.gameName ++ " | Waiting..."
                    , header = Nothing
                    , scheme = Just Theme.scheme3
                    }

                RejoinScreen welcome ->
                    { body = rejoinScreen model welcome
                    , title = welcome.gameName ++ " | Waiting..."
                    , header = Nothing
                    , scheme = Just Theme.scheme2
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
                                        " | your turn"

                                    Waiting ->
                                        " | waiting"

                                    Idle ->
                                        if self.isAdmin then
                                            " | end of round"

                                        else
                                            ""
                               )
                    , header = Nothing
                    , scheme = Just Theme.scheme2

                    -- TODO: what should this say
                    }

                RoundResultScreen potResults playerWinnings self game welcome ->
                    { body = roundResultsScreen model potResults playerWinnings self game welcome
                    , title = welcome.gameName ++ " | Round ended"
                    , header = Nothing
                    , scheme = Just Theme.scheme2
                    }

                GameResultScreen self game welcome ->
                    { body = gameResultsScreen model self game welcome
                    , title = welcome.gameName ++ " | Round ended"
                    , header = Nothing
                    , scheme = Just Theme.scheme2
                    }

                CommunityCardsScreen game welcome ->
                    { body = communityCardsUi game.round
                    , title = welcome.gameName
                    , header = Nothing
                    , scheme = Just Theme.scheme1
                    }

                TimerScreen timerStatus game welcome ->
                    { body = timerScreen model timerStatus game welcome
                    , title = welcome.gameName
                    , header = Nothing
                    , scheme = Just Theme.scheme1
                    }

                ChipSummaryScreen game welcome ->
                    { body = chipSummaryScreen model game welcome
                    , title = welcome.gameName
                    , header = Nothing
                    , scheme = Just Theme.scheme1
                    }

                UIElementsScreen seed act ->
                    { body = uiElements model seed act
                    , title = "Debugging | UI Elements"
                    , header = Nothing
                    , scheme = Just Theme.scheme2
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
            , Background.color <|
                Maybe.withDefault Theme.colours.white <|
                    Maybe.map .main page.scheme
            , inFront <|
                let
                    errorsEl =
                        if List.isEmpty model.errors then
                            Element.none

                        else
                            column
                                [ width fill
                                , paddingXY 10 20
                                , spacing 10
                                , alignBottom
                                , Background.color Theme.colours.error
                                , Border.widthEach
                                    { top = 5
                                    , bottom = 0
                                    , left = 0
                                    , right = 0
                                    }
                                , Border.color Theme.colours.black
                                , Font.color Theme.colours.white
                                ]
                            <|
                                List.map
                                    (\error -> paragraph [] [ text error.failure.message ])
                                <|
                                    List.reverse model.errors

                    overlayTemplate eventMessage =
                        column
                            [ width fill
                            , alignBottom
                            ]
                            [ el
                                [ width fill
                                , paddingEach
                                    { top = 0
                                    , bottom = 50
                                    , left = 0
                                    , right = 0
                                    }
                                ]
                              <|
                                column
                                    [ width fill
                                    , padding 16
                                    , Background.color Theme.colours.icon
                                    ]
                                    [ paragraph
                                        [ width shrink
                                        , centerX
                                        , paddingXY 8 2
                                        , Background.color Theme.colours.lowlight
                                        , Font.color <| Theme.textColour Theme.colours.white
                                        ]
                                        [ eventMessage
                                        ]
                                    ]
                            , errorsEl
                            ]

                    lookupPlayerName : PlayerId -> Maybe Player
                    lookupPlayerName pid =
                        case model.ui of
                            GameScreen _ _ game _ ->
                                lookupPlayer game.players pid

                            RoundResultScreen _ _ _ game _ ->
                                lookupPlayer game.players pid

                            GameResultScreen _ game _ ->
                                lookupPlayer game.players pid

                            CommunityCardsScreen game _ ->
                                lookupPlayer game.players pid

                            TimerScreen _ game _ ->
                                lookupPlayer game.players pid

                            ChipSummaryScreen game _ ->
                                lookupPlayer game.players pid

                            _ ->
                                Nothing

                    wasSelf pid =
                        case model.ui of
                            GameScreen _ self _ _ ->
                                pid == self.playerId

                            RoundResultScreen _ _ self _ _ ->
                                pid == self.playerId

                            GameResultScreen self _ _ ->
                                pid == self.playerId

                            _ ->
                                False
                in
                case List.Extra.last model.events of
                    Just event ->
                        case event.action of
                            GameStartedAction ->
                                overlayTemplate <| text "game started"

                            PlayerJoinedAction pid ->
                                if wasSelf pid then
                                    errorsEl

                                else
                                    overlayTemplate <|
                                        case lookupPlayerName pid of
                                            Just player ->
                                                row
                                                    []
                                                    [ el
                                                        [ Font.color Theme.colours.primary ]
                                                      <|
                                                        text player.screenName
                                                    , text " joined"
                                                    ]

                                            Nothing ->
                                                text "player joined"

                            CallAction pid ->
                                if wasSelf pid then
                                    errorsEl

                                else
                                    overlayTemplate <|
                                        case lookupPlayerName pid of
                                            Just player ->
                                                row
                                                    []
                                                    [ el
                                                        [ Font.color Theme.colours.primary ]
                                                      <|
                                                        text player.screenName
                                                    , text " called"
                                                    ]

                                            Nothing ->
                                                text "call"

                            BetAction pid betAmount ->
                                if wasSelf pid then
                                    errorsEl

                                else
                                    overlayTemplate <|
                                        case lookupPlayerName pid of
                                            Just player ->
                                                row
                                                    []
                                                    [ el
                                                        [ Font.color Theme.colours.primary ]
                                                      <|
                                                        text player.screenName
                                                    , text " bet "
                                                    , el
                                                        [ Font.color Theme.colours.highlightPrimary ]
                                                      <|
                                                        text <|
                                                            String.fromInt betAmount
                                                    ]

                                            Nothing ->
                                                text <| "bet " ++ String.fromInt betAmount

                            CheckAction pid ->
                                if wasSelf pid then
                                    errorsEl

                                else
                                    overlayTemplate <|
                                        case lookupPlayerName pid of
                                            Just player ->
                                                row
                                                    []
                                                    [ el
                                                        [ Font.color Theme.colours.primary ]
                                                      <|
                                                        text player.screenName
                                                    , text " checked"
                                                    ]

                                            Nothing ->
                                                text "check"

                            FoldAction pid ->
                                if wasSelf pid then
                                    errorsEl

                                else
                                    overlayTemplate <|
                                        case lookupPlayerName pid of
                                            Just player ->
                                                row
                                                    []
                                                    [ el
                                                        [ Font.color Theme.colours.primary ]
                                                      <|
                                                        text player.screenName
                                                    , text " folded"
                                                    ]

                                            Nothing ->
                                                text "fold"

                            AdvancePhaseAction ->
                                -- No need to display anything for this
                                errorsEl

                            TimerStatusAction playing ->
                                overlayTemplate <|
                                    if playing then
                                        text "timer started"

                                    else
                                        text "timer paused"

                            EditTimerAction ->
                                overlayTemplate <| text "timer updated"

                            EditBlindAction ->
                                overlayTemplate <| text "blind updated"

                            NoAction ->
                                errorsEl

                    Nothing ->
                        errorsEl
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
                            , padding 2
                            ]
                            [ case model.ui of
                                WelcomeScreen ->
                                    Element.none

                                _ ->
                                    Input.button
                                        [ padding 8
                                        , Background.color Theme.colours.lowlight
                                        , Font.size 20
                                        , Font.color <| Theme.textColour Theme.colours.white
                                        , Border.shadow
                                            { offset = ( 5, 5 )
                                            , size = 0
                                            , blur = 0
                                            , color = Theme.glow Theme.colours.lowlight
                                            }
                                        , focused
                                            [ Background.color <| Theme.focusColour Theme.colours.lowlight
                                            , Border.color Theme.colours.white
                                            , Border.shadow
                                                { offset = ( 5, 5 )
                                                , size = 0
                                                , blur = 0
                                                , color = Theme.glow <| Theme.focusColour Theme.colours.lowlight
                                                }
                                            ]
                                        ]
                                        { onPress = Just NavigateHome
                                        , label = text "home"
                                        }
                            , case model.ui of
                                GameScreen _ _ _ welcome ->
                                    row
                                        [ width fill
                                        , height fill
                                        , Font.color Theme.colours.primary
                                        , paddingXY 10 0
                                        , Font.size 12
                                        , clipX
                                        ]
                                        [ text welcome.screenName
                                        , text " "
                                        , html <|
                                            (FontAwesome.Solid.chevronRight
                                                |> FontAwesome.Icon.present
                                                |> FontAwesome.Icon.styled
                                                    [ FontAwesome.Attributes.xs
                                                    , FontAwesome.Attributes.fw
                                                    ]
                                                |> FontAwesome.Icon.view
                                            )
                                        , text " "
                                        , text welcome.gameName
                                        ]

                                _ ->
                                    Element.none
                            , el
                                [ alignRight
                                , padding 8
                                , Font.color <| Theme.textColour Theme.colours.white
                                ]
                              <|
                                case model.loadingStatus of
                                    AwaitingMessage ->
                                        html <|
                                            (FontAwesome.Solid.circleNotch
                                                |> FontAwesome.Icon.present
                                                |> FontAwesome.Icon.styled
                                                    [ FontAwesome.Attributes.sm
                                                    , FontAwesome.Attributes.fw
                                                    , FontAwesome.Attributes.spin
                                                    ]
                                                |> FontAwesome.Icon.withId "pokerdot_header-ui-loading"
                                                |> FontAwesome.Icon.titled "loading"
                                                |> FontAwesome.Icon.view
                                            )

                                    NotLoading ->
                                        Element.none
                            , el
                                [ alignRight
                                , padding 8
                                , Font.color <| Theme.textColour Theme.colours.white
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
    let
        scheme =
            Theme.scheme1
    in
    column
        [ width fill
        , height <| px 200
        , Background.color scheme.main
        , Border.color <| Theme.colours.black
        , Border.widthEach
            { bottom = 2
            , left = 0
            , right = 0
            , top = 0
            }
        , inFront <|
            el
                [ alignRight
                , padding 15
                , Font.color <| Theme.textColour Theme.colours.black
                , Element.Events.onClick <| NavigateUIElements 0
                ]
            <|
                connectionUi connected
        ]
        [ el
            [ width fill
            , centerY
            , Font.center
            , Font.bold
            , Font.size 60
            , Font.color <| Theme.textColour Theme.colours.black
            ]
          <|
            text "pokerdot"
        ]


welcomeScreen : Model -> Element Msg
welcomeScreen model =
    let
        scheme =
            Theme.scheme1
    in
    column
        [ width fill
        , height fill
        , centerX
        , spacing 48
        , paddingXY 0 50
        , Background.color scheme.main
        ]
        [ row
            [ centerX
            , width fill
            ]
            [ el
                [ centerX ]
              <|
                controlsButton scheme NavigateCreateGame <|
                    column
                        [ width fill
                        ]
                        [ el
                            [ width fill
                            , Font.center
                            ]
                          <|
                            text "create"
                        , el
                            [ width fill
                            , Font.center
                            ]
                          <|
                            text "game"
                        ]
            , el [ width <| px 30 ] Element.none
            , el [ centerX ] <|
                controlsButton scheme NavigateJoinGame <|
                    column
                        [ width fill
                        ]
                        [ el
                            [ width fill
                            , Font.center
                            ]
                          <|
                            text "join"
                        , el
                            [ width fill
                            , Font.center
                            ]
                          <|
                            text "game"
                        ]
            ]
        , if List.isEmpty model.library then
            Element.none

          else
            paragraph [ centerX ] [ text "Rejoin previous game" ]
        , container model.viewport <|
            column
                [ width fill
                , spacing 15
                ]
            <|
                List.map
                    (\welcomeMessage ->
                        row
                            [ width fill
                            , spacing 6
                            ]
                            [ Input.button
                                [ height <| px 40
                                , width <| px 40
                                , centerY
                                , alignRight
                                , Border.solid
                                , Border.width 2
                                , Border.color Theme.colours.black
                                , Border.shadow
                                    { offset = ( 5, 5 )
                                    , size = 0
                                    , blur = 0
                                    , color = Theme.glow Theme.colours.error
                                    }
                                , Background.color Theme.colours.error
                                , focused
                                    [ Background.color <| Theme.focusColour Theme.colours.error
                                    , Border.color Theme.colours.white
                                    , Font.color Theme.colours.white
                                    ]
                                ]
                                { onPress = Just <| DeletePersistedGame welcomeMessage
                                , label =
                                    el
                                        [ centerX
                                        , centerY
                                        ]
                                    <|
                                        html <|
                                            (FontAwesome.Solid.times
                                                |> FontAwesome.Icon.present
                                                |> FontAwesome.Icon.styled
                                                    [ FontAwesome.Attributes.sm
                                                    , FontAwesome.Attributes.fw
                                                    ]
                                                |> FontAwesome.Icon.withId ("pokerdot_library-rejoin-dismiss-" ++ welcomeMessage.gameCode)
                                                |> FontAwesome.Icon.titled "Remove game"
                                                |> FontAwesome.Icon.view
                                            )
                                }
                            , Input.button
                                [ width fill
                                , padding 5
                                , Border.solid
                                , Border.width 2
                                , Border.color Theme.colours.black
                                , Border.shadow
                                    { offset = ( 5, 5 )
                                    , size = 0
                                    , blur = 0
                                    , color = Theme.glow Theme.scheme2.main
                                    }
                                , Background.color Theme.scheme2.main
                                , Font.color Theme.colours.white
                                , focused
                                    [ Background.color <| Theme.focusColour Theme.scheme2.main
                                    , Border.color Theme.colours.white
                                    ]
                                ]
                                { onPress = Just <| NavigateGame welcomeMessage
                                , label =
                                    column
                                        [ spacing 5 ]
                                        [ row
                                            []
                                            [ el
                                                [ Font.color Theme.scheme2.highlight ]
                                              <|
                                                html <|
                                                    (FontAwesome.Solid.user
                                                        |> FontAwesome.Icon.present
                                                        |> FontAwesome.Icon.styled
                                                            [ FontAwesome.Attributes.sm
                                                            , FontAwesome.Attributes.fw
                                                            ]
                                                        |> FontAwesome.Icon.withId ("pokerdot_library-rejoin-player-" ++ welcomeMessage.gameCode)
                                                        |> FontAwesome.Icon.titled "Your name"
                                                        |> FontAwesome.Icon.view
                                                    )
                                            , text " "
                                            , text welcomeMessage.screenName
                                            ]
                                        , row
                                            []
                                            [ el
                                                [ Font.color Theme.scheme2.highlight ]
                                              <|
                                                html <|
                                                    (FontAwesome.Solid.gamepad
                                                        |> FontAwesome.Icon.present
                                                        |> FontAwesome.Icon.styled
                                                            [ FontAwesome.Attributes.sm
                                                            , FontAwesome.Attributes.fw
                                                            ]
                                                        |> FontAwesome.Icon.withId ("pokerdot_library-rejoin-game-" ++ welcomeMessage.gameCode)
                                                        |> FontAwesome.Icon.titled "Game name"
                                                        |> FontAwesome.Icon.view
                                                    )
                                            , text " "
                                            , text welcomeMessage.gameName
                                            ]
                                        ]
                                }
                            ]
                    )
                <|
                    List.reverse model.library
        ]


helpScreen : Model -> Element Msg
helpScreen model =
    Element.none


createGameScreen : Model -> String -> String -> Element Msg
createGameScreen model gameName screenName =
    container model.viewport <|
        column
            [ width fill
            , spacing 16
            , paddingEach { zWidths | top = 50 }
            ]
            [ pdText (\newGameName -> InputCreateGame newGameName screenName) gameName "game name"
            , pdText (InputCreateGame gameName) screenName "your name"
            , el
                [ alignRight
                ]
              <|
                controlsButton Theme.scheme3 (SubmitCreateGame gameName screenName) <|
                    column
                        [ width fill ]
                        [ el
                            [ width fill
                            , Font.center
                            ]
                          <|
                            text "create"
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
    container model.viewport <|
        column
            [ width fill
            , spacing 16
            , paddingEach { zWidths | top = 50 }
            ]
            [ if isExternal then
                Element.none

              else
                pdText (\newGameCode -> InputJoinGame isExternal newGameCode screenName) gameCode "game code"
            , pdText (InputJoinGame isExternal gameCode) screenName "your name"
            , el
                [ alignRight ]
              <|
                controlsButton Theme.scheme3 (SubmitJoinGame gameCode screenName) <|
                    column
                        [ width fill ]
                        [ el
                            [ width fill
                            , Font.center
                            ]
                          <|
                            text "join"
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
        requiredMissingPlayer =
            row
                [ width fill
                , padding 8
                , Font.color <| Theme.textColour Theme.colours.black
                , Background.color Theme.colours.error
                ]
                [ el
                    [ Font.color <| Theme.textColour Theme.colours.white ]
                  <|
                    html <|
                        (FontAwesome.Solid.userPlus
                            |> FontAwesome.Icon.present
                            |> FontAwesome.Icon.styled [ FontAwesome.Attributes.sm ]
                            |> FontAwesome.Icon.view
                        )
                , text " "
                ]

        missingPlayer =
            row
                [ width fill
                , padding 8
                , Font.color <| Theme.textColour Theme.colours.black
                ]
                [ el
                    [ Font.color <| Theme.textColour Theme.colours.white ]
                  <|
                    html <|
                        (FontAwesome.Solid.userPlus
                            |> FontAwesome.Icon.present
                            |> FontAwesome.Icon.styled [ FontAwesome.Attributes.xs ]
                            |> FontAwesome.Icon.view
                        )
                , text " "
                ]

        formatPlayer : Player -> Element Msg
        formatPlayer player =
            row
                [ width fill
                , padding 8
                , Font.color <| Theme.textColour Theme.colours.black
                , Background.color Theme.colours.primary
                ]
                [ el
                    [ Font.color Theme.colours.lowlight ]
                  <|
                    html <|
                        (FontAwesome.Solid.userCircle
                            |> FontAwesome.Icon.present
                            |> FontAwesome.Icon.styled [ FontAwesome.Attributes.sm ]
                            |> FontAwesome.Icon.view
                        )
                , text " "
                , text player.screenName
                , if player.playerId == self.playerId then
                    text " (you)"

                  else
                    Element.none
                ]
    in
    container model.viewport <|
        column
            [ width fill
            , spacing 16
            , paddingEach { zWidths | top = 15 }
            ]
            [ row
                [ width fill
                , spacing 15
                ]
                [ el
                    [ height fill
                    , padding 15
                    , alignLeft
                    , Font.size 40
                    , Font.alignLeft
                    , Background.color Theme.colours.highlightPrimary
                    , Border.solid
                    , Border.color Theme.colours.lowlight
                    , Border.widthEach
                        { top = 0
                        , bottom = 3
                        , left = 0
                        , right = 0
                        }
                    ]
                  <|
                    text <|
                        game.gameCode
                , column
                    [ width fill
                    , spacing 12
                    , Font.color <| Theme.textColour Theme.colours.white
                    ]
                    [ el
                        [ width fill
                        , Font.alignLeft
                        , Font.size 20
                        ]
                      <|
                        paragraph
                            [ width fill ]
                            [ text "other players can use this code to join your game"
                            , text ", or use the link below"
                            ]
                    , link
                        [ Font.color Theme.colours.icon ]
                        { url = "/#join/" ++ game.gameCode
                        , label =
                            row
                                [ spacing 4 ]
                                [ el
                                    []
                                  <|
                                    html <|
                                        (FontAwesome.Solid.shareAlt
                                            |> FontAwesome.Icon.present
                                            |> FontAwesome.Icon.styled [ FontAwesome.Attributes.sm ]
                                            |> FontAwesome.Icon.withId "share-join-link_pokerdot"
                                            |> FontAwesome.Icon.titled "Share join link"
                                            |> FontAwesome.Icon.view
                                        )
                                , text ""
                                , el
                                    [ Font.underline ]
                                  <|
                                    text "join link"
                                ]
                        }
                    ]
                ]
            , column
                [ width fill
                , padding 8
                , spacing 2
                , Background.color Theme.colours.lowlight
                ]
              <|
                List.concat
                    [ List.map formatPlayer playerOrder
                    , List.repeat
                        (max 0 <| 2 - List.length playerOrder)
                        requiredMissingPlayer
                    , [ missingPlayer ]
                    ]
            , if self.isAdmin then
                lobbyStartSettings playerOrder chipsSettings

              else
                none
            ]


lobbyStartSettings : List Player -> ChipsSettings -> Element Msg
lobbyStartSettings playerOrder chipsSettings =
    column
        [ width fill
        , spacing 15
        ]
        [ row
            [ width fill ]
            [ el
                [ alignRight ]
              <|
                controlsButton Theme.scheme3 SubmitStartGame (text "start")
            ]
        , column
            [ width fill
            , spacing 6
            , padding 8
            , Background.color Theme.colours.lowlight
            ]
            [ row
                [ spacing 8
                , moveUp 11
                , moveLeft 4
                ]
                [ pdTab
                    (case chipsSettings of
                        TrackWithManualBlinds _ _ ->
                            True

                        _ ->
                            False
                    )
                    (InputStartGameSettings playerOrder <| TrackWithManualBlinds 1000 5)
                    "manual blinds"
                , pdTab
                    (case chipsSettings of
                        TrackWithTimer _ _ ->
                            True

                        _ ->
                            False
                    )
                    (InputStartGameSettings playerOrder <| TrackWithTimer 1000 [])
                    "timer"
                , pdTab
                    (case chipsSettings of
                        DoNotTrackChips ->
                            True

                        _ ->
                            False
                    )
                    (InputStartGameSettings playerOrder DoNotTrackChips)
                    "no chips"
                ]
            , case chipsSettings of
                DoNotTrackChips ->
                    el
                        [ Font.color <| Theme.textColour Theme.colours.white
                        , Font.size 18
                        ]
                    <|
                        text "games without chips do not work yet"

                TrackWithTimer currentStackSize timerLevels ->
                    column
                        [ width fill
                        , spacing 8
                        ]
                        [ paragraph
                            [ width fill
                            , Font.size 18
                            , Font.alignLeft
                            , Font.color <| Theme.textColour Theme.colours.white
                            ]
                            [ text "using a game timer to track blinds does not yet work" ]
                        , Input.text
                            [ Font.alignLeft
                            , paddingXY 10 8
                            , Border.solid
                            , Border.width 2
                            , Border.color Theme.colours.black
                            , Border.widthEach { zWidths | bottom = 2 }
                            , Border.rounded 0
                            , Background.color Theme.colours.white
                            , focused
                                [ Background.color Theme.colours.highlightPrimary
                                ]
                            ]
                            { text = String.fromInt currentStackSize
                            , label =
                                Input.labelLeft
                                    [ width <| px 150
                                    , paddingXY 8 0
                                    , Font.alignRight
                                    , Font.color <| Theme.textColour Theme.colours.white
                                    ]
                                <|
                                    text "player stacks"
                            , placeholder =
                                Just <|
                                    Input.placeholder
                                        []
                                    <|
                                        text "player stacks"
                            , onChange =
                                \value ->
                                    let
                                        stackSize =
                                            1
                                    in
                                    InputStartGameSettings playerOrder <|
                                        TrackWithTimer stackSize timerLevels
                            }
                        , el
                            [ Font.color <| Theme.textColour Theme.colours.white
                            ]
                          <|
                            text "timer levels"
                        ]

                TrackWithManualBlinds currentStackSize initialSmallBlind ->
                    column
                        [ width fill
                        , spacing 4
                        ]
                        [ Input.text
                            [ Font.alignLeft
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
                            ]
                            { text = String.fromInt currentStackSize
                            , label =
                                Input.labelLeft
                                    [ width <| px 150
                                    , paddingXY 8 0
                                    , Font.alignRight
                                    , Font.color <| Theme.textColour Theme.colours.white
                                    ]
                                <|
                                    text "player stacks"
                            , placeholder =
                                Just <| Input.placeholder [] <| text "player stacks"
                            , onChange =
                                \value ->
                                    let
                                        stackSize =
                                            Maybe.withDefault 0 <| String.toInt value
                                    in
                                    InputStartGameSettings playerOrder <|
                                        TrackWithManualBlinds stackSize initialSmallBlind
                            }
                        , row
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
                                { text = String.fromInt initialSmallBlind
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
                                        InputStartGameSettings playerOrder <|
                                            TrackWithManualBlinds currentStackSize smallBlind
                                }
                            , row
                                [ Font.color <| Theme.textColour Theme.colours.white ]
                                [ text " / "
                                , text <| String.fromInt (initialSmallBlind * 2)
                                ]
                            ]
                        ]
            ]
        ]


rejoinScreen : Model -> Welcome -> Element Msg
rejoinScreen model welcome =
    container model.viewport <|
        column
            [ width fill
            , spacing 16
            , paddingEach { zWidths | top = 15 }
            ]
            [ el
                [ height fill
                , padding 15
                , alignLeft
                , Font.size 40
                , Font.alignLeft
                , Background.color Theme.colours.highlightPrimary
                , Border.solid
                , Border.color Theme.colours.lowlight
                , Border.widthEach
                    { top = 0
                    , bottom = 3
                    , left = 0
                    , right = 0
                    }
                ]
              <|
                text welcome.gameName
            , paragraph
                [ width shrink
                , padding 4
                , Background.color Theme.colours.primary
                ]
                [ text "rejoining as "
                , el
                    [ Font.color Theme.scheme1.highlight
                    , Font.bold
                    ]
                  <|
                    text welcome.screenName
                , text " "
                , el
                    [ Font.color Theme.colours.lowlight ]
                  <|
                    html <|
                        (FontAwesome.Solid.circleNotch
                            |> FontAwesome.Icon.present
                            |> FontAwesome.Icon.styled
                                [ FontAwesome.Attributes.sm
                                , FontAwesome.Attributes.fw
                                , FontAwesome.Attributes.spin
                                ]
                            |> FontAwesome.Icon.withId "pokerdot_rejoin-spinner-"
                            |> FontAwesome.Icon.titled "loading"
                            |> FontAwesome.Icon.view
                        )
                ]
            ]


gameScreen : Model -> PlayingState -> ActSelection -> Self -> Game -> Welcome -> Element Msg
gameScreen model playingState currentAct self game welcome =
    container model.viewport <|
        column
            [ width fill
            , spacing 16
            , paddingEach { zWidths | top = 15 }
            ]
            [ paragraph
                [ width shrink
                , paddingXY 8 2
                , Background.color Theme.colours.highlightPrimary
                , Font.color <| Theme.textColour Theme.colours.lowlight
                , Font.bold
                ]
                [ text game.gameName ]
            , tableUi game.round game.players
            , selfUi model.peeking self
            , case playingState of
                Playing ->
                    pokerControlsUi True game.smallBlind currentAct self game.players

                Waiting ->
                    pokerControlsUi False game.smallBlind NoAct self game.players

                Idle ->
                    if self.isAdmin then
                        column
                            [ width fill ]
                            [ el
                                [ alignRight ]
                              <|
                                controlsButton Theme.scheme1 AdvancePhase <|
                                    column
                                        [ width fill ]
                                        [ el
                                            [ width fill
                                            , Font.center
                                            ]
                                          <|
                                            text "next"
                                        , el
                                            [ width fill
                                            , Font.center
                                            ]
                                          <|
                                            text "round"
                                        ]
                            , pokerControlsUi False game.smallBlind NoAct self game.players
                            ]

                    else
                        pokerControlsUi False game.smallBlind NoAct self game.players
            ]


roundResultsScreen : Model -> List PotResult -> List PlayerWinnings -> Self -> Game -> Welcome -> Element Msg
roundResultsScreen model potResults playerWinnings self game welcome =
    column
        [ width fill
        , spacing 16
        , paddingEach { zWidths | top = 15 }
        ]
        [ container model.viewport <|
            paragraph
                [ width shrink
                , paddingXY 8 2
                , Background.color Theme.colours.highlightPrimary
                , Font.color <| Theme.textColour Theme.colours.lowlight
                , Font.bold
                ]
                [ text game.gameName ]
        , container model.viewport <| tableUi game.round game.players
        , container model.viewport <| selfUi model.peeking self
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
                        [ Maybe.withDefault none <| Maybe.map (handUi model.viewport name pw.winnings (Just pw.hole)) pw.hand ]
                )
                playerWinnings
        , container model.viewport <|
            if self.isAdmin then
                let
                    nextBlind =
                        game.smallBlind * 2
                in
                row
                    [ width fill ]
                    [ el
                        [ alignLeft ]
                      <|
                        controlsButton Theme.scheme1
                            (UpdateBlind nextBlind)
                        <|
                            column
                                [ width fill
                                , spacing 5
                                ]
                                [ el
                                    [ width fill
                                    , Font.center
                                    ]
                                  <|
                                    text "blinds"
                                , el
                                    [ width fill
                                    , Font.center
                                    , Font.size 18
                                    ]
                                  <|
                                    text (String.fromInt nextBlind ++ " / " ++ String.fromInt (nextBlind * 2))
                                ]
                    , el
                        [ alignRight ]
                      <|
                        controlsButton Theme.scheme1 AdvancePhase <|
                            column
                                [ width fill ]
                                [ el
                                    [ width fill
                                    , Font.center
                                    ]
                                  <|
                                    text "next"
                                , el
                                    [ width fill
                                    , Font.center
                                    ]
                                  <|
                                    text "round"
                                ]
                    ]

            else
                Element.none
        ]


gameResultsScreen : Model -> Self -> Game -> Welcome -> Element Msg
gameResultsScreen model self game welcome =
    container model.viewport <|
        column
            [ width fill
            , spacing 16
            , paddingEach { zWidths | top = 15 }
            ]
            [ container model.viewport <|
                paragraph
                    [ width shrink
                    , paddingXY 8 2
                    , Background.color Theme.colours.highlightPrimary
                    , Font.color <| Theme.textColour Theme.colours.lowlight
                    , Font.bold
                    ]
                    [ text game.gameName ]
            , tableUi game.round game.players
            , selfUi model.peeking self
            ]


timerScreen : Model -> TimerStatus -> Game -> Welcome -> Element Msg
timerScreen model timerStatus game welcome =
    Element.none


chipSummaryScreen : Model -> Game -> Welcome -> Element Msg
chipSummaryScreen model game welcome =
    Element.none
