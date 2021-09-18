module Messages exposing (lookupPlayer, parsePersistedGames, routeFromUi, routeFromUrl, sendPing, sendWake, uiFromRoute, update)

import Browser exposing (UrlRequest(..))
import Browser.Dom
import Browser.Navigation
import Json.Decode exposing (errorToString)
import List.Extra
import Logic exposing (gameIsFinished)
import Model exposing (ActSelection(..), Action(..), AdvancePhaseRequest, BetRequest, CheckRequest, ChipsSettings(..), CreateGameRequest, EditBlindsSettings(..), Event, Failure, FoldRequest, Game, JoinGameRequest, LoadingStatus(..), Message(..), Model, Msg(..), PingRequest, Player, PlayerId(..), Route(..), Self, StartGameRequest, UI(..), UpdateBlindRequest, Welcome, advancePhaseRequestEncoder, betRequestEncoder, checkRequestEncoder, createGameRequestEncoder, defaultChipSettings, foldRequestEncoder, getPlayerCode, joinGameRequestEncoder, messageDecoder, persistedWelcomeDecoder, pingRequestEncoder, startGameRequestEncoder, updateBlindRequestEncoder, wakeRequestEncoder, welcomeEncoder)
import Ports exposing (deletePersistedGame, persistNewGame, reportError, requestPersistedGames, sendMessage)
import Task
import Time
import Url
import Url.Builder
import Url.Parser exposing ((</>))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Tick posix ->
            let
                filter life error =
                    Time.posixToMillis error.time + life > Time.posixToMillis posix

                -- automatically remove errors after a certain amount of time has elapsed
                errorLifeMs =
                    6000

                filteredErrors =
                    List.filter (filter errorLifeMs) model.errors

                -- automatically remove events after a certain amount of time has elapsed
                eventLifeMs =
                    6000

                filteredEvents =
                    List.filter (filter eventLifeMs) model.events
            in
            ( { model
                | now = posix
                , errors = filteredErrors
                , events = filteredEvents
              }
            , Cmd.none
            )

        OnResize ->
            ( model
            , Task.perform Resized Browser.Dom.getViewport
            )

        Resized viewport ->
            ( { model
                | viewport = viewport
              }
            , Cmd.none
            )

        UrlRequest urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Browser.Navigation.pushUrl model.navKey <| Url.toString url
                    )

                External href ->
                    ( model
                    , Browser.Navigation.load href
                    )

        UrlChange url ->
            let
                currentRoute =
                    routeFromUi model.ui

                urlRoute =
                    routeFromUrl url

                ( ui, cmds ) =
                    if currentRoute /= urlRoute then
                        -- navigating to a different screen
                        let
                            newUi =
                                uiFromRoute urlRoute model.library

                            cmd =
                                case newUi of
                                    RejoinScreen welcome ->
                                        sendPing welcome

                                    _ ->
                                        Cmd.none
                        in
                        ( newUi, cmd )

                    else
                        -- url matches current state so we're all good
                        ( model.ui, Cmd.none )
            in
            ( { model | ui = ui }
            , cmds
            )

        ServerMessage json ->
            case Json.Decode.decodeValue (messageDecoder model.now) json of
                Ok (WelcomeMessage welcome self game) ->
                    let
                        newLibrary =
                            if List.member welcome model.library then
                                model.library

                            else
                                welcome :: model.library

                        savedGameJson =
                            welcomeEncoder welcome

                        gameRoute =
                            GameRoute welcome.gameCode (getPlayerCode welcome.playerId)
                    in
                    case model.ui of
                        CreateGameScreen gameName _ ->
                            if gameName == welcome.gameName then
                                ( { model
                                    | library = newLibrary
                                    , ui = LobbyScreen game.players defaultChipSettings self game welcome
                                    , loadingStatus = NotLoading
                                  }
                                , Cmd.batch
                                    [ navigate model.navKey False gameRoute
                                    , persistNewGame savedGameJson
                                    ]
                                )

                            else
                                -- different game, background update
                                ( { model | library = newLibrary }
                                , persistNewGame savedGameJson
                                )

                        JoinGameScreen external gameCode _ ->
                            if gameCode == welcome.gameCode then
                                ( { model
                                    | library = newLibrary
                                    , ui = LobbyScreen game.players defaultChipSettings self game welcome
                                    , loadingStatus = NotLoading
                                  }
                                , Cmd.batch
                                    [ navigate model.navKey False gameRoute
                                    , persistNewGame savedGameJson
                                    ]
                                )

                            else
                                -- different game, background update
                                ( { model | library = newLibrary }
                                , persistNewGame savedGameJson
                                )

                        _ ->
                            ( { model | library = newLibrary }
                            , persistNewGame savedGameJson
                            )

                Ok (PlayerGameStatusMessage self game action) ->
                    case model.ui of
                        WelcomeScreen ->
                            -- must have left the game before the server responded
                            ( registerEvent model action
                            , Cmd.none
                            )

                        HelpScreen ->
                            -- must have navigated away from the game before the server responded
                            ( registerEvent model action
                            , Cmd.none
                            )

                        CreateGameScreen _ _ ->
                            -- must have left the game before the server responded
                            ( registerEvent model action
                            , Cmd.none
                            )

                        JoinGameScreen _ _ _ ->
                            -- must have left the game before the server responded
                            ( registerEvent model action
                            , Cmd.none
                            )

                        LobbyScreen oldPlayers chipsSettings oldSelf oldGame welcome ->
                            if game.gameId == oldGame.gameId then
                                case action of
                                    GameStartedAction ->
                                        ( { model
                                            | ui = GameScreen NoAct self game welcome
                                            , events = addAction model action
                                            , loadingStatus = NotLoading
                                            , peeking = False
                                          }
                                        , Cmd.none
                                        )

                                    PlayerJoinedAction newPlayerId ->
                                        let
                                            players =
                                                includeAllPlayers oldPlayers game.players
                                        in
                                        ( { model
                                            | ui = LobbyScreen players chipsSettings self game welcome
                                            , events = addAction model action
                                          }
                                        , Cmd.none
                                        )

                                    _ ->
                                        ( registerEvent model action
                                        , Cmd.none
                                        )

                            else
                                ( registerEvent model action
                                , Cmd.none
                                )

                        RejoinScreen welcome ->
                            let
                                modelWithEvent =
                                    registerEvent model action

                                ui =
                                    if game.started then
                                        -- TODO: work out correct ui from game state
                                        GameScreen NoAct self game welcome

                                    else
                                        LobbyScreen game.players defaultChipSettings self game welcome
                            in
                            ( { modelWithEvent
                                | ui = ui
                                , loadingStatus = NotLoading
                                , peeking = False
                              }
                            , Cmd.none
                            )

                        GameScreen actSelection oldSelf oldGame welcome ->
                            let
                                updatedModel =
                                    -- Ignore message if it isn't for the current game
                                    if oldGame.gameId == game.gameId then
                                        { model
                                            | ui = GameScreen actSelection self game welcome
                                            , loadingStatus = NotLoading
                                            , peeking =
                                                -- new hole is dealt face-down
                                                if self.hole /= oldSelf.hole then
                                                    False

                                                else
                                                    model.peeking
                                        }

                                    else
                                        model
                            in
                            ( registerEvent updatedModel action
                            , Cmd.none
                            )

                        RoundResultScreen potResults playerWinnings oldSelf _ welcome blindsSettings ->
                            let
                                updatedModel =
                                    case game.inTurn of
                                        -- new round
                                        Just _ ->
                                            { model
                                                | ui = GameScreen NoAct self game welcome
                                                , loadingStatus = NotLoading
                                                , peeking =
                                                    -- new hole is dealt face-down
                                                    if self.hole /= oldSelf.hole then
                                                        False

                                                    else
                                                        model.peeking
                                            }

                                        -- stay on results if a status message happens to come in while the round results are being displayed
                                        Nothing ->
                                            -- if we've successfully updated the blinds, then we can close the blinds editor
                                            let
                                                newUi =
                                                    case action of
                                                        TimerStatusAction _ ->
                                                            RoundResultScreen potResults playerWinnings self game welcome DoNotEditBlinds

                                                        EditTimerAction ->
                                                            RoundResultScreen potResults playerWinnings self game welcome DoNotEditBlinds

                                                        EditBlindAction ->
                                                            RoundResultScreen potResults playerWinnings self game welcome DoNotEditBlinds

                                                        _ ->
                                                            RoundResultScreen potResults playerWinnings self game welcome blindsSettings
                                            in
                                            { model
                                                | ui = newUi
                                                , loadingStatus = NotLoading
                                            }
                            in
                            ( registerEvent updatedModel action
                            , Cmd.none
                            )

                        CommunityCardsScreen _ welcome ->
                            let
                                newUi =
                                    CommunityCardsScreen game welcome

                                updatedModel =
                                    { model | ui = newUi }
                            in
                            ( registerEvent updatedModel action
                            , Cmd.none
                            )

                        TimerScreen timerStatus _ welcome ->
                            let
                                newUi =
                                    TimerScreen timerStatus game welcome

                                updatedModel =
                                    { model | ui = newUi }
                            in
                            ( registerEvent updatedModel action
                            , Cmd.none
                            )

                        ChipSummaryScreen _ welcome ->
                            let
                                newUi =
                                    ChipSummaryScreen game welcome

                                updatedModel =
                                    { model | ui = newUi }
                            in
                            ( registerEvent updatedModel action
                            , Cmd.none
                            )

                        UIElementsScreen _ _ ->
                            ( registerEvent model action
                            , Cmd.none
                            )

                Ok (SpectatorGameStatusMessage spectator game action) ->
                    ( registerEvent model action
                    , Cmd.none
                    )

                Ok (PlayerRoundWinningsMessage self game pots playerWinnings) ->
                    let
                        newUi =
                            case welcomeFromUi model.ui of
                                Nothing ->
                                    model.ui

                                Just welcome ->
                                    RoundResultScreen pots playerWinnings self game welcome DoNotEditBlinds
                    in
                    ( registerEvent
                        { model
                            | ui = newUi
                            , loadingStatus = NotLoading
                        }
                        AdvancePhaseAction
                    , Cmd.none
                    )

                Ok (SpectatorRoundWinningsMessage spectator game pots players) ->
                    ( model, Cmd.none )

                Ok (StatusMessage message) ->
                    ( model
                    , Cmd.none
                    )

                Ok (FailureMessage failures) ->
                    let
                        updatedModel =
                            displayFailures model failures
                    in
                    ( updatedModel, Cmd.none )

                Err error ->
                    ( displayFailure
                        { message = "There was an error understanding a message from the server"
                        , context = Nothing
                        }
                        model
                    , reportError <| "JSON error for server response: " ++ Json.Decode.errorToString error
                    )

        SocketConnect ->
            let
                newModel =
                    { model | connected = True }
            in
            case model.ui of
                WelcomeScreen ->
                    ( newModel, Cmd.none )

                HelpScreen ->
                    ( newModel, Cmd.none )

                CreateGameScreen gameName screenName ->
                    ( newModel, Cmd.none )

                JoinGameScreen external gameCode screenName ->
                    ( newModel, Cmd.none )

                LobbyScreen players chipsSettings self game welcome ->
                    ( newModel
                    , Cmd.none
                    )

                RejoinScreen welcome ->
                    ( newModel
                    , sendPing welcome
                    )

                GameScreen actSelection self game welcome ->
                    ( newModel
                    , sendPing welcome
                    )

                RoundResultScreen potResults playerWinnings self game welcome _ ->
                    ( newModel
                    , sendPing welcome
                    )

                CommunityCardsScreen game welcome ->
                    ( newModel
                    , sendPing welcome
                    )

                TimerScreen timerStatus game welcome ->
                    ( newModel
                    , sendPing welcome
                    )

                ChipSummaryScreen game welcome ->
                    ( newModel
                    , sendPing welcome
                    )

                UIElementsScreen _ _ ->
                    ( newModel, Cmd.none )

        SocketDisconnect ->
            ( { model | connected = False }
            , Cmd.none
            )

        NavigateHome ->
            ( { model
                | ui = WelcomeScreen
                , errors = []
                , loadingStatus = NotLoading
              }
            , Cmd.batch
                [ navigate model.navKey True HomeRoute
                , requestPersistedGames ()
                ]
            )

        NavigateHelp ->
            ( model
            , navigate model.navKey True HelpRoute
            )

        NavigateGame welcome ->
            ( { model | ui = RejoinScreen welcome }
            , Cmd.batch
                [ sendPing welcome
                , navigate model.navKey True (GameRoute welcome.gameCode (getPlayerCode welcome.playerId))
                ]
            )

        UpdateLibrary json ->
            let
                ( newLibrary, parseLibraryCmd ) =
                    parsePersistedGames json
            in
            ( { model | library = newLibrary }
            , parseLibraryCmd
            )

        PersistGame welcomeMessage ->
            let
                json =
                    welcomeEncoder welcomeMessage
            in
            ( model
            , persistNewGame json
            )

        DeletePersistedGame welcome ->
            let
                json =
                    welcomeEncoder welcome

                filteredLibrary =
                    List.Extra.filterNot
                        (\game ->
                            game.gameId == welcome.gameId && game.playerKey == welcome.playerKey
                        )
                        model.library
            in
            ( { model
                | library = filteredLibrary
              }
            , deletePersistedGame json
            )

        RequestPersistedGames ->
            ( model
            , requestPersistedGames ()
            )

        NavigateCreateGame ->
            ( { model | ui = CreateGameScreen "" "" }
            , navigate model.navKey True CreateRoute
            )

        InputCreateGame gameName screenName ->
            ( { model | ui = CreateGameScreen gameName screenName }
            , Cmd.none
            )

        SubmitCreateGame gameName screenName ->
            let
                request =
                    { gameName = gameName
                    , screenName = screenName
                    }
            in
            ( { model | loadingStatus = AwaitingMessage }
            , sendCreateGame request
            )

        NavigateJoinGame ->
            ( { model | ui = JoinGameScreen False "" "" }
            , navigate model.navKey True (JoinRoute Nothing)
            )

        InputJoinGame external gameCode screenName ->
            ( { model | ui = JoinGameScreen external gameCode screenName }
            , Cmd.none
            )

        SubmitJoinGame gameCode screenName ->
            let
                request =
                    { gameCode = gameCode
                    , screenName = screenName
                    }
            in
            ( { model | loadingStatus = AwaitingMessage }
            , sendJoinGame request
            )

        InputStartGameSettings newPlayers chipSettings ->
            case model.ui of
                LobbyScreen prevPlayers oldChipsSettings self game welcome ->
                    let
                        players =
                            includeAllPlayers newPlayers prevPlayers
                    in
                    ( { model | ui = LobbyScreen players chipSettings self game welcome }
                    , Cmd.none
                    )

                _ ->
                    ( displayFailure
                        (failureMessage "You can only reorder players from the lobby.")
                        model
                    , Cmd.none
                    )

        SubmitStartGame ->
            case model.ui of
                LobbyScreen players chipsSettings self game welcome ->
                    let
                        request =
                            case chipsSettings of
                                DoNotTrackChips ->
                                    { gameId = welcome.gameId
                                    , playerId = welcome.playerId
                                    , playerKey = welcome.playerKey
                                    , startingStack = Nothing
                                    , initialSmallBlind = Nothing
                                    , timerConfig = Nothing
                                    , playerOrder = List.map .playerId players
                                    }

                                TrackWithTimer initialStackSize timerLevels ->
                                    { gameId = welcome.gameId
                                    , playerId = welcome.playerId
                                    , playerKey = welcome.playerKey
                                    , startingStack = Just initialStackSize
                                    , initialSmallBlind = Nothing
                                    , timerConfig = Just timerLevels
                                    , playerOrder = List.map .playerId players
                                    }

                                TrackWithManualBlinds initialStackSize initialSmallBlind ->
                                    { gameId = welcome.gameId
                                    , playerId = welcome.playerId
                                    , playerKey = welcome.playerKey
                                    , startingStack = Just initialStackSize
                                    , initialSmallBlind = Just initialSmallBlind
                                    , timerConfig = Nothing
                                    , playerOrder = List.map .playerId players
                                    }
                    in
                    ( model
                    , sendStartGame request
                    )

                _ ->
                    ( displayFailure
                        { message = "You can only start games from the lobby"
                        , context = Nothing
                        }
                        model
                    , Cmd.none
                    )

        TogglePeek ->
            ( { model | peeking = not model.peeking }
            , Cmd.none
            )

        Check ->
            case model.ui of
                GameScreen _ self game welcome ->
                    ( { model
                        | loadingStatus = AwaitingMessage
                        , ui = GameScreen NoAct self game welcome
                      }
                    , sendCheckRequest
                        { gameId = welcome.gameId
                        , playerKey = welcome.playerKey
                        , playerId = welcome.playerId
                        }
                    )

                _ ->
                    ( displayFailure
                        (failureMessage "It isn't your turn to check")
                        model
                    , Cmd.none
                    )

        InputActSelection actSelection ->
            case model.ui of
                GameScreen _ self game welcome ->
                    ( { model
                        | ui = GameScreen actSelection self game welcome
                      }
                    , Cmd.none
                    )

                -- Allow interaction on the components screen
                UIElementsScreen seed _ ->
                    ( { model
                        | ui = UIElementsScreen seed actSelection
                      }
                    , Cmd.none
                    )

                _ ->
                    ( displayFailure
                        (failureMessage "You are not playing a game")
                        model
                    , Cmd.none
                    )

        InputBet amount ->
            let
                normalisedAmount =
                    max 0 amount
            in
            case model.ui of
                GameScreen _ self game welcome ->
                    ( { model
                        | ui = GameScreen (ActBet normalisedAmount) self game welcome
                      }
                    , Cmd.none
                    )

                UIElementsScreen seed _ ->
                    ( { model
                        | ui = UIElementsScreen seed <| ActBet normalisedAmount
                      }
                    , Cmd.none
                    )

                _ ->
                    ( displayFailure
                        (failureMessage "You must be playing a game to select a bet")
                        model
                    , Cmd.none
                    )

        Bet amount ->
            case model.ui of
                GameScreen _ self game welcome ->
                    ( { model
                        | loadingStatus = AwaitingMessage
                        , ui = GameScreen NoAct self game welcome
                      }
                    , sendBetRequest
                        { gameId = welcome.gameId
                        , playerKey = welcome.playerKey
                        , playerId = welcome.playerId
                        , betAmount = amount
                        }
                    )

                _ ->
                    ( displayFailure
                        (failureMessage "It isn't your turn to bet")
                        model
                    , Cmd.none
                    )

        Fold ->
            case model.ui of
                GameScreen _ self game welcome ->
                    ( { model
                        | loadingStatus = AwaitingMessage
                        , ui = GameScreen NoAct self game welcome
                      }
                    , sendFoldRequest
                        { gameId = welcome.gameId
                        , playerKey = welcome.playerKey
                        , playerId = welcome.playerId
                        }
                    )

                _ ->
                    ( displayFailure
                        (failureMessage "It isn't your turn to fold")
                        model
                    , Cmd.none
                    )

        AdvancePhase ->
            case model.ui of
                GameScreen _ _ _ welcome ->
                    ( { model | loadingStatus = AwaitingMessage }
                    , sendAdvancePhaseRequest
                        { gameId = welcome.gameId
                        , playerKey = welcome.playerKey
                        , playerId = welcome.playerId
                        }
                    )

                RoundResultScreen _ _ _ _ welcome _ ->
                    ( { model | loadingStatus = AwaitingMessage }
                    , sendAdvancePhaseRequest
                        { gameId = welcome.gameId
                        , playerKey = welcome.playerKey
                        , playerId = welcome.playerId
                        }
                    )

                _ ->
                    ( displayFailure
                        (failureMessage "We can't advance to the next phase yet")
                        model
                    , Cmd.none
                    )

        InputUpdateBlind blindsSettings ->
            case model.ui of
                RoundResultScreen potResults playerWinnings self game welcome _ ->
                    ( { model | ui = RoundResultScreen potResults playerWinnings self game welcome blindsSettings }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        UpdateBlind blindsSettings ->
            case blindsSettings of
                DoNotEditBlinds ->
                    ( model, Cmd.none )

                DoNotTrackBlinds ->
                    case model.ui of
                        GameScreen _ _ _ welcome ->
                            ( { model | loadingStatus = AwaitingMessage }
                            , sendUpdateBlind
                                { gameId = welcome.gameId
                                , playerKey = welcome.playerKey
                                , playerId = welcome.playerId
                                , timerLevels = Nothing
                                , smallBlind = Nothing
                                , playing = Nothing
                                }
                            )

                        RoundResultScreen _ _ _ _ welcome _ ->
                            ( { model | loadingStatus = AwaitingMessage }
                            , sendUpdateBlind
                                { gameId = welcome.gameId
                                , playerKey = welcome.playerKey
                                , playerId = welcome.playerId
                                , timerLevels = Nothing
                                , smallBlind = Nothing
                                , playing = Nothing
                                }
                            )

                        _ ->
                            ( model, Cmd.none )

                ManualBlinds blindAmount ->
                    case model.ui of
                        GameScreen _ _ _ welcome ->
                            ( { model | loadingStatus = AwaitingMessage }
                            , sendUpdateBlind
                                { gameId = welcome.gameId
                                , playerKey = welcome.playerKey
                                , playerId = welcome.playerId
                                , timerLevels = Nothing
                                , smallBlind = Just blindAmount
                                , playing = Nothing
                                }
                            )

                        RoundResultScreen _ _ _ _ welcome _ ->
                            ( { model | loadingStatus = AwaitingMessage }
                            , sendUpdateBlind
                                { gameId = welcome.gameId
                                , playerKey = welcome.playerKey
                                , playerId = welcome.playerId
                                , timerLevels = Nothing
                                , smallBlind = Just blindAmount
                                , playing = Nothing
                                }
                            )

                        _ ->
                            ( model, Cmd.none )

                TimerBlinds timerLevels ->
                    -- TODO: support timers
                    ( model, Cmd.none )

        NavigateUIElements seed ->
            ( { model | ui = UIElementsScreen seed NoAct }
            , Cmd.none
            )


failureMessage : String -> Failure
failureMessage message =
    { message = message
    , context = Nothing
    }


displayFailure : Failure -> Model -> Model
displayFailure failure model =
    let
        error =
            { failure = failure
            , time = model.now
            }
    in
    { model
        | errors = error :: model.errors
    }


registerEvent : Model -> Action -> Model
registerEvent model action =
    let
        event =
            { action = action
            , time = model.now
            }
    in
    if action == NoAction then
        model

    else
        { model
            | events = event :: model.events
        }


addAction : Model -> Action -> List Event
addAction model action =
    let
        event =
            { action = action
            , time = model.now
            }
    in
    if action == NoAction then
        model.events

    else
        event :: model.events


displayFailures : Model -> List Failure -> Model
displayFailures model failures =
    List.foldl displayFailure model failures


parsePersistedGames : Json.Decode.Value -> ( List Welcome, Cmd msg )
parsePersistedGames json =
    let
        persistedGamesResult =
            Json.Decode.decodeValue (Json.Decode.list persistedWelcomeDecoder) json
    in
    case persistedGamesResult of
        Ok persistedGames ->
            ( persistedGames, Cmd.none )

        Err decodeError ->
            ( []
            , reportError <|
                errorToString decodeError
            )


includeAllPlayers : List Player -> List Player -> List Player
includeAllPlayers p1s p2s =
    List.Extra.uniqueBy
        (\p ->
            case p.playerId of
                Pid pid ->
                    pid
        )
    <|
        List.append p1s p2s


lookupPlayer : List Player -> PlayerId -> Maybe Player
lookupPlayer players playerId =
    List.Extra.find (\p -> p.playerId == playerId) players


welcomeFromUi : UI -> Maybe Welcome
welcomeFromUi ui =
    case ui of
        WelcomeScreen ->
            Nothing

        HelpScreen ->
            Nothing

        CreateGameScreen _ _ ->
            Nothing

        JoinGameScreen _ _ _ ->
            Nothing

        LobbyScreen _ _ _ _ welcome ->
            Just welcome

        RejoinScreen welcome ->
            Just welcome

        GameScreen _ _ _ welcome ->
            Just welcome

        RoundResultScreen _ _ _ _ welcome _ ->
            Just welcome

        CommunityCardsScreen _ welcome ->
            Just welcome

        TimerScreen _ _ welcome ->
            Just welcome

        ChipSummaryScreen _ welcome ->
            Just welcome

        UIElementsScreen _ _ ->
            Nothing



-- routing


routeFromUi : UI -> Route
routeFromUi ui =
    case ui of
        WelcomeScreen ->
            HomeRoute

        HelpScreen ->
            HelpRoute

        CreateGameScreen _ _ ->
            CreateRoute

        JoinGameScreen external gameCode _ ->
            -- it should be possible to link to a game with the gameCode
            if external then
                JoinRoute <| Just gameCode

            else
                JoinRoute Nothing

        LobbyScreen _ _ _ _ welcome ->
            GameRoute
                welcome.gameCode
                (getPlayerCode welcome.playerId)

        RejoinScreen welcome ->
            GameRoute
                welcome.gameCode
                (getPlayerCode welcome.playerId)

        GameScreen _ _ _ welcome ->
            GameRoute
                welcome.gameCode
                (getPlayerCode welcome.playerId)

        CommunityCardsScreen _ welcome ->
            GameRoute
                welcome.gameCode
                (getPlayerCode welcome.playerId)

        ChipSummaryScreen _ welcome ->
            GameRoute
                welcome.gameCode
                (getPlayerCode welcome.playerId)

        TimerScreen _ _ welcome ->
            GameRoute
                welcome.gameCode
                (getPlayerCode welcome.playerId)

        RoundResultScreen _ _ _ _ welcome _ ->
            GameRoute
                welcome.gameCode
                (getPlayerCode welcome.playerId)

        UIElementsScreen _ _ ->
            UiElementsRoute


routeFromUrl : Url.Url -> Route
routeFromUrl rawUrl =
    let
        -- treat fragment as path
        url =
            { rawUrl
                | path = Maybe.withDefault "" rawUrl.fragment
                , fragment = Nothing
            }

        homeParser =
            Url.Parser.map HomeRoute Url.Parser.top

        gameParser =
            Url.Parser.map
                GameRoute
                (Url.Parser.s "game" </> Url.Parser.string </> Url.Parser.string)

        helpParser =
            Url.Parser.map
                HelpRoute
                (Url.Parser.s "help")

        createParser =
            Url.Parser.map
                CreateRoute
                (Url.Parser.s "new")

        joinInternalParser =
            Url.Parser.map
                (JoinRoute Nothing)
                (Url.Parser.s "join")

        joinExternalParser =
            Url.Parser.map
                (\s -> JoinRoute <| Just s)
                (Url.Parser.s "join" </> Url.Parser.string)

        uiElementsParser =
            Url.Parser.map
                UiElementsRoute
                (Url.Parser.s "ui-elements")

        -- etc
        routeParser =
            Url.Parser.oneOf
                [ homeParser
                , gameParser
                , helpParser
                , createParser
                , joinInternalParser
                , joinExternalParser
                , uiElementsParser
                ]
    in
    Url.Parser.parse routeParser url
        |> Maybe.withDefault NotFound


uiFromRoute : Route -> List Welcome -> UI
uiFromRoute route library =
    case route of
        HomeRoute ->
            WelcomeScreen

        HelpRoute ->
            HelpScreen

        CreateRoute ->
            CreateGameScreen "" ""

        JoinRoute Nothing ->
            JoinGameScreen False "" ""

        JoinRoute (Just gameCode) ->
            JoinGameScreen True gameCode ""

        GameRoute gameCode playerCode ->
            -- check for matching welcome in library
            List.Extra.find
                (\welcome ->
                    String.startsWith gameCode welcome.gameCode
                        && String.startsWith playerCode (getPlayerCode welcome.playerId)
                )
                library
                |> Maybe.map RejoinScreen
                |> Maybe.withDefault WelcomeScreen

        NotFound ->
            WelcomeScreen

        UiElementsRoute ->
            UIElementsScreen 0 NoAct


navigate : Browser.Navigation.Key -> Bool -> Route -> Cmd Msg
navigate navKey withHistory route =
    let
        newUrl =
            case route of
                HomeRoute ->
                    Url.Builder.custom Url.Builder.Relative [] [] <| Just ""

                HelpRoute ->
                    Url.Builder.custom Url.Builder.Relative [] [] <| Just "help"

                CreateRoute ->
                    Url.Builder.custom Url.Builder.Relative [] [] <| Just "new"

                JoinRoute (Just gameCode) ->
                    Url.Builder.custom Url.Builder.Relative
                        []
                        []
                    <|
                        Just ("join/" ++ gameCode)

                JoinRoute Nothing ->
                    Url.Builder.custom Url.Builder.Relative
                        []
                        []
                    <|
                        Just "join"

                GameRoute gameCode playerCode ->
                    Url.Builder.custom Url.Builder.Relative
                        []
                        []
                    <|
                        Just ("game/" ++ gameCode ++ "/" ++ playerCode)

                NotFound ->
                    Url.Builder.custom Url.Builder.Relative [] [] <| Just ""

                UiElementsRoute ->
                    Url.Builder.custom Url.Builder.Relative
                        []
                        []
                    <|
                        Just "ui-elements"
    in
    if withHistory then
        Browser.Navigation.pushUrl navKey newUrl

    else
        Browser.Navigation.replaceUrl navKey newUrl



-- server requests


sendCreateGame : CreateGameRequest -> Cmd Msg
sendCreateGame createGameRequest =
    sendMessage <| createGameRequestEncoder createGameRequest


sendJoinGame : JoinGameRequest -> Cmd Msg
sendJoinGame joinGameRequest =
    sendMessage <| joinGameRequestEncoder joinGameRequest


sendStartGame : StartGameRequest -> Cmd Msg
sendStartGame startGameRequest =
    sendMessage <| startGameRequestEncoder startGameRequest


sendUpdateBlind : UpdateBlindRequest -> Cmd Msg
sendUpdateBlind updateBlindRequest =
    sendMessage <| updateBlindRequestEncoder updateBlindRequest


sendBetRequest : BetRequest -> Cmd Msg
sendBetRequest betRequest =
    sendMessage <| betRequestEncoder betRequest


sendCheckRequest : CheckRequest -> Cmd Msg
sendCheckRequest checkRequest =
    sendMessage <| checkRequestEncoder checkRequest


sendFoldRequest : FoldRequest -> Cmd Msg
sendFoldRequest foldRequest =
    sendMessage <| foldRequestEncoder foldRequest


sendAdvancePhaseRequest : AdvancePhaseRequest -> Cmd Msg
sendAdvancePhaseRequest advancePhaseRequest =
    sendMessage <| advancePhaseRequestEncoder advancePhaseRequest


sendPing : Welcome -> Cmd Msg
sendPing welcome =
    let
        pingRequest =
            { playerId = welcome.playerId
            , playerKey = welcome.playerKey
            , gameId = welcome.gameId
            }
    in
    sendMessage <| pingRequestEncoder pingRequest


sendWake : () -> Cmd Msg
sendWake _ =
    sendMessage <| wakeRequestEncoder ()
