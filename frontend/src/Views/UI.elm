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
import Html.Attributes
import List.Extra
import Logic exposing (gameIsFinished, winner)
import Maybe.Extra
import Messages exposing (lookupPlayer)
import Model exposing (ActSelection(..), Action(..), Card, ChipsSettings(..), EditBlindsSettings(..), Game, Hand(..), LoadingStatus(..), Model, Msg(..), OverlayUI(..), Player, PlayerId, PlayerWinnings, PlayingState(..), PotResult, Round(..), Self, TextInput, TimerLevel(..), TimerStatus, UI(..), Welcome)
import Timers exposing (CurrentTimerLevel(..), defaultStack, defaultTimerLevels)
import Utils exposing (swapDown, swapUp)
import Views.Elements
    exposing
        ( blindUi
        , buttonHiddenAttrs
        , communityCardsUi
        , connectionUi
        , container
        , controlsButton
        , editTimerUi
        , handUi
        , helpText
        , logo
        , pdTab
        , pdText
        , pokerControlsUi
        , rejoinFromLibraryUi
        , selfUi
        , tableUi
        , uiElements
        , zWidths
        )
import Views.Overlays exposing (editBlindsOverlay, helpOverlay, overlayTemplate)
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
        page : Page
        page =
            case model.ui of
                WelcomeScreen ->
                    { body = welcomeScreen model
                    , title = "pokerdot"
                    , header =
                        Just <| welcomeHeader model.connected
                    , scheme = Just Theme.scheme1
                    }

                HelpScreen ->
                    { body = helpScreen model
                    , title = "help"
                    , header = Nothing
                    , scheme = Just Theme.scheme4
                    }

                CreateGameScreen gameName screenName ->
                    { body = createGameScreen model gameName screenName
                    , title = "new game"
                    , header = Nothing
                    , scheme = Just Theme.scheme3
                    }

                JoinGameScreen external gameCode screenName ->
                    { body = joinGameScreen model external gameCode screenName
                    , title = "join game"
                    , header = Nothing
                    , scheme = Just Theme.scheme3
                    }

                LobbyScreen players chipsSettings self game welcome ->
                    { body = lobbyScreen model players chipsSettings self game welcome
                    , title = welcome.gameName ++ " | waiting..."
                    , header = Nothing
                    , scheme = Just Theme.scheme3
                    }

                RejoinScreen welcome ->
                    { body = rejoinScreen model welcome
                    , title = welcome.gameName ++ " | waiting..."
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

                RoundResultScreen potResults playerWinnings self game welcome blindsSettings ->
                    { body = roundResultsScreen model potResults playerWinnings self game welcome blindsSettings
                    , title =
                        welcome.gameName
                            ++ (if gameIsFinished game then
                                    " | game ended"

                                else
                                    " | round ended"
                               )
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
                    , title = "debugging | UI elements"
                    , header = Nothing
                    , scheme = Just Theme.scheme2
                    }

        overlay =
            case model.overlayUi of
                NoOverlay ->
                    Nothing

                HelpOverlay ->
                    Just <|
                        overlayTemplate model.viewport "help?" helpOverlay

                EditBlindOverlay editBlindsSettings ->
                    case model.ui of
                        GameScreen actSelection self game welcome ->
                            Just <|
                                overlayTemplate model.viewport "edit blinds" <|
                                    editBlindsOverlay model.now
                                        (List.sum <| List.map (\p -> p.stack + p.pot + p.bet) game.players)
                                        (List.length game.players)
                                        game.smallBlind
                                        game.timer
                                        editBlindsSettings

                        RoundResultScreen potResults playerWinnings self game welcome _ ->
                            Just <|
                                overlayTemplate model.viewport "edit blinds" <|
                                    editBlindsOverlay model.now
                                        (List.sum <| List.map (\p -> p.stack + p.pot + p.bet) game.players)
                                        (List.length game.players)
                                        game.smallBlind
                                        game.timer
                                        editBlindsSettings

                        _ ->
                            Just <|
                                overlayTemplate model.viewport "edit blinds" <|
                                    paragraph []
                                        [ text "you cannot edit blinds when you are not playing a game"
                                        ]
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
                Maybe.withDefault Theme.colours.primary <|
                    Maybe.map .main page.scheme
            , htmlAttribute <| Html.Attributes.style "transition" "background .2s ease-in"
            , inFront <|
                let
                    maybeErrorsEl =
                        if List.isEmpty model.errors then
                            Nothing

                        else
                            Just <|
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

                    maybeEventEl =
                        Maybe.andThen
                            (\event ->
                                let
                                    container eventMessage =
                                        column
                                            [ width fill
                                            , paddingXY 16 8
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
                                in
                                case event.action of
                                    GameStartedAction ->
                                        Just <| container <| text "game started"

                                    PlayerJoinedAction pid ->
                                        if wasSelf pid then
                                            Nothing

                                        else
                                            Just <|
                                                container <|
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
                                            Nothing

                                        else
                                            Just <|
                                                container <|
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
                                            Nothing

                                        else
                                            Just <|
                                                container <|
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
                                            Nothing

                                        else
                                            Just <|
                                                container <|
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
                                            Nothing

                                        else
                                            Just <|
                                                container <|
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
                                        Nothing

                                    TimerStatusAction playing ->
                                        Just <|
                                            container <|
                                                if playing then
                                                    text "timer started"

                                                else
                                                    text "timer paused"

                                    EditTimerAction ->
                                        Just <| container <| text "blinds timer updated"

                                    EditBlindAction ->
                                        let
                                            newBlindAmount =
                                                case model.ui of
                                                    LobbyScreen _ _ _ game _ ->
                                                        String.fromInt game.smallBlind ++ " / " ++ String.fromInt (game.smallBlind * 2)

                                                    GameScreen _ _ game _ ->
                                                        String.fromInt game.smallBlind ++ " / " ++ String.fromInt (game.smallBlind * 2)

                                                    RoundResultScreen _ _ _ game _ _ ->
                                                        String.fromInt game.smallBlind ++ " / " ++ String.fromInt (game.smallBlind * 2)

                                                    CommunityCardsScreen game _ ->
                                                        String.fromInt game.smallBlind ++ " / " ++ String.fromInt (game.smallBlind * 2)

                                                    TimerScreen _ game _ ->
                                                        String.fromInt game.smallBlind ++ " / " ++ String.fromInt (game.smallBlind * 2)

                                                    ChipSummaryScreen game _ ->
                                                        String.fromInt game.smallBlind ++ " / " ++ String.fromInt (game.smallBlind * 2)

                                                    _ ->
                                                        ""
                                        in
                                        Just <|
                                            container <|
                                                paragraph
                                                    []
                                                    [ text "blinds updated "
                                                    , el
                                                        [ Font.color <| Theme.textColour Theme.colours.primary ]
                                                      <|
                                                        text newBlindAmount
                                                    ]

                                    NoAction ->
                                        Nothing
                            )
                        <|
                            List.Extra.last model.events

                    lookupPlayerName : PlayerId -> Maybe Player
                    lookupPlayerName pid =
                        case model.ui of
                            GameScreen _ _ game _ ->
                                lookupPlayer game.players pid

                            RoundResultScreen _ _ _ game _ _ ->
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

                            RoundResultScreen _ _ self _ _ _ ->
                                pid == self.playerId

                            _ ->
                                False

                    eventElYOffset =
                        45
                in
                case ( maybeEventEl, maybeErrorsEl ) of
                    ( Just eventEl, Nothing ) ->
                        el
                            [ width fill
                            , moveDown eventElYOffset
                            ]
                            eventEl

                    ( Nothing, Just errorsEl ) ->
                        el
                            [ width fill
                            , alignBottom
                            ]
                            errorsEl

                    ( Just eventEl, Just errorsEl ) ->
                        column
                            [ width fill
                            , height fill

                            -- this prevents the overlay itself (between events and errors) blocking interaction
                            , htmlAttribute <| Html.Attributes.style "pointer-events" "none"
                            ]
                            [ el
                                [ width fill
                                , moveDown eventElYOffset

                                -- restore interactions for the events content
                                , htmlAttribute <| Html.Attributes.style "pointer-events" "auto"
                                ]
                                eventEl
                            , el
                                [ width fill
                                , alignBottom

                                -- restore interactions for the errors content
                                , htmlAttribute <| Html.Attributes.style "pointer-events" "auto"
                                ]
                                errorsEl
                            ]

                    ( Nothing, Nothing ) ->
                        Element.none
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
                            , spacing 8
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
                                        , Border.color <| Theme.colours.lowlight
                                        , Border.width 2
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
                                        , mouseOver
                                            [ Background.color <| Theme.focusColour Theme.colours.lowlight
                                            , Border.color <| Theme.focusColour Theme.colours.lowlight
                                            ]
                                        ]
                                        { onPress = Just NavigateHome
                                        , label =
                                            row
                                                [ spacing 8 ]
                                                [ el
                                                    [ Background.color Theme.colours.primary
                                                    , Border.rounded 6
                                                    ]
                                                  <|
                                                    logo 18
                                                , text "home"
                                                ]
                                        }
                            , Input.button
                                [ padding 8
                                , Background.color Theme.colours.lowlight
                                , Font.size 20
                                , Font.color <| Theme.textColour Theme.colours.white
                                , Border.color <| Theme.colours.lowlight
                                , Border.width 2
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
                                , mouseOver
                                    [ Background.color <| Theme.focusColour Theme.colours.lowlight
                                    , Border.color <| Theme.focusColour Theme.colours.lowlight
                                    ]
                                ]
                                { onPress = Just OpenHelpOverlay
                                , label =
                                    row
                                        [ spacing 8 ]
                                        [ text "?"
                                        ]
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
                                , Font.color <| Theme.textColour Theme.colours.highlightPrimary
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
                                , Font.color <| Theme.textColour Theme.colours.lowlight
                                ]
                              <|
                                connectionUi model.connected
                            ]

                    Just headerEl ->
                        headerEl

                -- body & overlay
                , el
                    (case overlay of
                        Nothing ->
                            [ width fill ]

                        Just overlayElement ->
                            [ width fill
                            , inFront overlayElement
                            ]
                    )
                    page.body
                , el
                    [ height <| px 10
                    ]
                    Element.none
                ]
        ]
    }


welcomeHeader : Bool -> Element Msg
welcomeHeader connected =
    el
        [ width fill
        , Border.color <| Theme.colours.lowlight
        , Border.widthEach
            { bottom = 2
            , left = 0
            , right = 0
            , top = 0
            }
        ]
    <|
        column
            [ width fill
            , spacing 25
            , paddingXY 0 25
            , Border.color <| Theme.colours.highlightPrimary
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
            [ row
                [ centerY
                , centerX
                , spacing 4
                ]
                [ el
                    []
                  <|
                    logo 100
                , el
                    [ centerY
                    , Font.center
                    , Font.bold
                    , Font.size 40
                    , Font.color <| Theme.textColour Theme.colours.black
                    ]
                  <|
                    text "pokerdot"
                ]
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
        , container model.viewport <|
            rejoinFromLibraryUi model.now <|
                List.reverse model.library
        ]


helpScreen : Model -> Element Msg
helpScreen model =
    Element.none


createGameScreen : Model -> TextInput -> TextInput -> Element Msg
createGameScreen model gameNameInput screenNameInput =
    container model.viewport <|
        column
            [ width fill
            , spacing 16
            , paddingEach { zWidths | top = 50 }
            ]
            [ pdText CreateGameInputGameName gameNameInput "game name"
            , pdText CreateGameInputScreenName screenNameInput "your name"
            , el
                [ alignRight
                ]
              <|
                controlsButton Theme.scheme3 (SubmitCreateGame gameNameInput.value screenNameInput.value) <|
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
            , helpText
                [ "creates a new game that others can join. "
                , "you will be able to start the game when players have joined"
                ]
            ]


joinGameScreen : Model -> Bool -> TextInput -> TextInput -> Element Msg
joinGameScreen model isExternal gameCodeInput screenNameInput =
    container model.viewport <|
        column
            [ width fill
            , spacing 16
            , paddingEach { zWidths | top = 50 }
            ]
            [ if isExternal then
                Element.none

              else
                pdText JoinGameInputGameCode gameCodeInput "game code"
            , pdText JoinGameInputScreenName screenNameInput "your name"
            , el
                [ alignRight ]
              <|
                controlsButton Theme.scheme3 (SubmitJoinGame gameCodeInput.value screenNameInput.value) <|
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
            , helpText
                [ "join an existing game. "
                , if isExternal then
                    ""

                  else
                    "the game's creator can tell you the game code"
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
            let
                last =
                    Maybe.withDefault False <|
                        Maybe.map
                            (\p -> p == player)
                        <|
                            List.Extra.last playerOrder

                first =
                    Maybe.withDefault False <|
                        Maybe.map
                            (\p -> p == player)
                        <|
                            List.head playerOrder
            in
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
                , el
                    [ width fill
                    , clip
                    , Font.alignLeft
                    ]
                  <|
                    if player.playerId == self.playerId then
                        text (player.screenName ++ " (you)")

                    else
                        text player.screenName
                , row
                    [ alignRight
                    , spacing 8
                    ]
                    [ el
                        [ width <| px 0
                        , height <| px 30
                        ]
                        Element.none
                    , if self.isAdmin then
                        Input.button
                            ([ width <| px 30
                             , height <| px 30
                             , alignRight
                             , Border.rounded 2
                             , Border.solid
                             , Border.width 2
                             , Border.color Theme.colours.black
                             , Border.shadow
                                { offset = ( 3, 3 )
                                , size = 0
                                , blur = 0
                                , color = Theme.glow Theme.colours.lowlight
                                }
                             , Font.size 25
                             , Font.color <| Theme.textColour Theme.colours.white
                             , Background.color Theme.colours.lowlight
                             , focused
                                [ Background.color <| Theme.focusColour Theme.colours.lowlight
                                , Border.color Theme.colours.white
                                , Border.shadow
                                    { offset = ( 3, 3 )
                                    , size = 0
                                    , blur = 0
                                    , color = Theme.glow <| Theme.focusColour Theme.colours.lowlight
                                    }
                                ]
                             , mouseOver
                                [ Background.color <| Theme.focusColour Theme.colours.lowlight ]
                             ]
                                ++ buttonHiddenAttrs first
                            )
                            { onPress = Just <| InputStartGameSettings (swapUp player playerOrder) chipsSettings
                            , label =
                                html <|
                                    (FontAwesome.Solid.caretUp
                                        |> FontAwesome.Icon.present
                                        |> FontAwesome.Icon.styled [ FontAwesome.Attributes.sm ]
                                        |> FontAwesome.Icon.withId ("pokerdot-reorder-players-up_pokerdot_" ++ player.screenName)
                                        |> FontAwesome.Icon.titled "Move up"
                                        |> FontAwesome.Icon.view
                                    )
                            }

                      else
                        Element.none
                    , if self.isAdmin then
                        Input.button
                            ([ width <| px 30
                             , height <| px 30
                             , alignRight
                             , Border.rounded 2
                             , Border.solid
                             , Border.width 2
                             , Border.color Theme.colours.black
                             , Border.shadow
                                { offset = ( 3, 3 )
                                , size = 0
                                , blur = 0
                                , color = Theme.glow Theme.colours.lowlight
                                }
                             , Font.size 25
                             , Font.color <| Theme.textColour Theme.colours.white
                             , Background.color Theme.colours.lowlight
                             , focused
                                [ Background.color <| Theme.focusColour Theme.colours.lowlight
                                , Border.color Theme.colours.white
                                , Border.shadow
                                    { offset = ( 3, 3 )
                                    , size = 0
                                    , blur = 0
                                    , color = Theme.glow <| Theme.focusColour Theme.colours.lowlight
                                    }
                                ]
                             , mouseOver
                                [ Background.color <| Theme.focusColour Theme.colours.lowlight ]
                             ]
                                ++ buttonHiddenAttrs last
                            )
                            { onPress = Just <| InputStartGameSettings (swapDown player playerOrder) chipsSettings
                            , label =
                                html <|
                                    (FontAwesome.Solid.caretDown
                                        |> FontAwesome.Icon.present
                                        |> FontAwesome.Icon.styled [ FontAwesome.Attributes.sm ]
                                        |> FontAwesome.Icon.withId ("pokerdot-reorder-players-down_pokerdot_" ++ player.screenName)
                                        |> FontAwesome.Icon.titled "Move down"
                                        |> FontAwesome.Icon.view
                                    )
                            }

                      else
                        Element.none
                    ]
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
                controlsButton Theme.scheme3 SubmitStartGame <|
                    column
                        [ width fill
                        , spacing 8
                        ]
                        [ el
                            [ width fill
                            , centerX
                            ]
                          <|
                            text "start"
                        , el
                            [ Font.color <| Theme.textColour Theme.colours.white
                            , centerX
                            ]
                          <|
                            html <|
                                (FontAwesome.Solid.play
                                    |> FontAwesome.Icon.present
                                    |> FontAwesome.Icon.styled
                                        [ FontAwesome.Attributes.xs
                                        ]
                                    |> FontAwesome.Icon.view
                                )
                        ]
            ]
        , el
            [ height <| px 4 ]
            Element.none
        , column
            [ width fill
            , spacing 6
            , padding 8
            , Background.color Theme.colours.lowlight
            ]
            [ row
                [ spacing 10
                , moveUp 24
                , moveLeft 4
                ]
                [ pdTab Theme.colours.cta
                    (case chipsSettings of
                        TrackWithManualBlinds _ _ ->
                            True

                        _ ->
                            False
                    )
                    (InputStartGameSettings playerOrder <| TrackWithManualBlinds 1000 5)
                    "manual blinds"
                , pdTab Theme.colours.cta
                    (case chipsSettings of
                        TrackWithTimer _ _ ->
                            True

                        _ ->
                            False
                    )
                    (InputStartGameSettings playerOrder <| TrackWithTimer defaultStack (defaultTimerLevels (List.length playerOrder)))
                    "timer"

                --, pdTab
                --    (case chipsSettings of
                --        DoNotTrackChips ->
                --            True
                --
                --        _ ->
                --            False
                --    )
                --    (InputStartGameSettings playerOrder DoNotTrackChips)
                --    "no chips"
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
                        [ el
                            [ width fill
                            , Font.color <| Theme.textColour Theme.colours.white
                            ]
                          <|
                            editTimerUi
                                (\startingStack tls -> InputStartGameSettings playerOrder <| TrackWithTimer startingStack tls)
                                timerLevels
                                (List.length playerOrder)
                                currentStackSize
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
            [ row
                [ width fill
                , spacing 4
                ]
                [ el
                    [ width fill
                    , alignBottom
                    ]
                  <|
                    paragraph
                        [ width shrink
                        , paddingXY 8 2
                        , Background.color Theme.colours.highlightPrimary
                        , Font.alignLeft
                        , Font.color <| Theme.textColour Theme.colours.lowlight
                        , Font.bold
                        ]
                        [ text game.gameName ]
                , el
                    [ width shrink
                    ]
                  <|
                    blindUi model.now game.timer game.smallBlind self.isAdmin
                ]
            , tableUi game.round game.button Nothing game.inTurn game.players
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
                                    case game.round of
                                        PreFlopRound ->
                                            column
                                                [ width fill ]
                                                [ el
                                                    [ width fill
                                                    , Font.center
                                                    ]
                                                  <|
                                                    text "deal"
                                                ]

                                        FlopRound _ _ _ ->
                                            column
                                                [ width fill ]
                                                [ el
                                                    [ width fill
                                                    , Font.center
                                                    ]
                                                  <|
                                                    text "deal"
                                                ]

                                        TurnRound _ _ _ _ ->
                                            column
                                                [ width fill ]
                                                [ el
                                                    [ width fill
                                                    , Font.center
                                                    ]
                                                  <|
                                                    text "deal"
                                                ]

                                        RiverRound _ _ _ _ _ ->
                                            column
                                                [ width fill ]
                                                [ el
                                                    [ width fill
                                                    , Font.center
                                                    ]
                                                  <|
                                                    text "see"
                                                , el
                                                    [ width fill
                                                    , Font.center
                                                    ]
                                                  <|
                                                    text "results"
                                                ]

                                        ShowdownRound _ _ _ _ _ _ ->
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


roundResultsScreen : Model -> List PotResult -> List PlayerWinnings -> Self -> Game -> Welcome -> EditBlindsSettings -> Element Msg
roundResultsScreen model potResults playerWinnings self game welcome blindsSettings =
    let
        maybeWinningPlayer =
            winner game
    in
    column
        [ width fill
        , spacing 16
        , paddingEach { zWidths | top = 15 }
        ]
        [ container model.viewport <|
            row
                [ width fill
                , spacing 4
                ]
                [ el
                    [ width fill ]
                  <|
                    paragraph
                        [ width shrink
                        , paddingXY 8 2
                        , Background.color Theme.colours.highlightPrimary
                        , Font.alignLeft
                        , Font.color <| Theme.textColour Theme.colours.lowlight
                        , Font.bold
                        ]
                        [ text game.gameName ]
                , el
                    [ width shrink
                    ]
                  <|
                    blindUi model.now game.timer game.smallBlind self.isAdmin
                ]
        , container model.viewport <| tableUi game.round game.button (Just playerWinnings) game.inTurn game.players
        , container model.viewport <| selfUi model.peeking self
        , case maybeWinningPlayer of
            Just winningPlayer ->
                el
                    [ width fill
                    , paddingXY 0 25
                    , Background.color Theme.colours.primary
                    , Font.alignLeft
                    ]
                <|
                    container model.viewport <|
                        paragraph
                            []
                            [ el
                                [ padding 2
                                , Background.color Theme.colours.icon
                                ]
                              <|
                                text winningPlayer.screenName
                            , text " is the winner"
                            ]

            Nothing ->
                Element.none
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
            if self.isAdmin && Maybe.Extra.isNothing maybeWinningPlayer then
                column
                    [ width fill
                    , spacing 18
                    ]
                    [ row
                        [ width fill ]
                        [ controlsButton Theme.scheme3
                            OpenEditBlindOverlay
                            (column
                                [ width fill ]
                                [ el
                                    [ width fill
                                    , Font.center
                                    ]
                                  <|
                                    text "edit"
                                , el
                                    [ width fill
                                    , Font.center
                                    ]
                                  <|
                                    text "blinds"
                                ]
                            )
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
                    , Font.alignLeft
                    , Font.color <| Theme.textColour Theme.colours.lowlight
                    , Font.bold
                    ]
                    [ text game.gameName ]
            , tableUi game.round game.button Nothing game.inTurn game.players
            , selfUi model.peeking self
            ]


timerScreen : Model -> TimerStatus -> Game -> Welcome -> Element Msg
timerScreen model timerStatus game welcome =
    Element.none


chipSummaryScreen : Model -> Game -> Welcome -> Element Msg
chipSummaryScreen model game welcome =
    Element.none
