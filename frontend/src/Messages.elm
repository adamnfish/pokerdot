module Messages exposing (followRoute, routeFromUi, routeFromUrl, sendWake, update)

import Browser.Dom
import Browser.Navigation
import Json.Decode exposing (errorToString)
import List.Extra
import Model exposing (ChipsSettings(..), CreateGameRequest, Failure, JoinGameRequest, LoadingStatus(..), Message(..), Model, Msg(..), PingRequest, Route(..), StartGameRequest, UI(..), Welcome, createGameRequestEncoder, getGameCode, getPlayerCode, joinGameRequestEncoder, messageDecoder, pingRequestEncoder, startGameRequestEncoder, wakeRequestEncoder, welcomeDecoder, welcomeEncoder)
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
                -- automatically remove errors after a certain amount of time has elapsed
                errorLifeMs =
                    6000

                filteredErrors =
                    List.filter
                        (\error ->
                            Time.posixToMillis error.time + errorLifeMs > Time.posixToMillis posix
                        )
                        model.errors
            in
            ( { model
                | now = posix
                , errors = filteredErrors
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
            ( model, Cmd.none )

        UrlChange url ->
            let
                currentRoute =
                    routeFromUi model.ui

                urlRoute =
                    routeFromUrl url

                ( ui, cmds ) =
                    if currentRoute /= urlRoute then
                        -- navigating to a different screen
                        followRoute model.navKey urlRoute model.library

                    else
                        -- url matches current state so we're all good
                        ( model.ui, Cmd.none )
            in
            ( { model
                | url = url
                , ui = ui
              }
            , cmds
            )

        ServerMessage json ->
            let
                parsedMessage =
                    Json.Decode.decodeValue messageDecoder json
            in
            case parsedMessage of
                Ok (WelcomeMessage welcome game) ->
                    let
                        newLibrary =
                            if List.member welcome model.library then
                                model.library

                            else
                                welcome :: model.library
                    in
                    case model.ui of
                        CreateGameScreen gameName _ ->
                            if gameName == welcome.gameName then
                                ( { model
                                    | library = newLibrary
                                    , ui = LobbyScreen game.players DoNotTrackChips game welcome
                                  }
                                , Cmd.none
                                )

                            else
                                -- different game, background update
                                ( { model | library = newLibrary }
                                , Cmd.none
                                )

                        JoinGameScreen external gameCode _ ->
                            if gameCode == getGameCode welcome.gameId then
                                ( { model
                                    | library = newLibrary
                                    , ui = LobbyScreen game.players DoNotTrackChips game welcome
                                  }
                                , Cmd.none
                                )

                            else
                                -- different game, background update
                                ( { model | library = newLibrary }
                                , Cmd.none
                                )

                        -- TODO: handle the lobby case where status message arrives before welcome
                        _ ->
                            ( { model | library = newLibrary }
                            , Cmd.none
                            )

                Ok (PlayerGameStatusMessage self game action) ->
                    ( model, Cmd.none )

                Ok (PlayerRoundWinningsMessage self game pots players) ->
                    ( model, Cmd.none )

                Ok (SpectatorGameStatusMessage spectator game action) ->
                    ( model, Cmd.none )

                Ok (SpectatorRoundWinningsMessage spectator game pots players) ->
                    ( model, Cmd.none )

                Ok (StatusMessage string) ->
                    ( model, Cmd.none )

                Ok (FailureMessage failures) ->
                    let
                        updatedModel =
                            displayFailures model failures
                    in
                    ( updatedModel, Cmd.none )

                Err error ->
                    ( model, Cmd.none )

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

                LobbyScreen players chipsSettings game welcome ->
                    ( newModel
                    , Cmd.none
                    )

                RejoinScreen welcome ->
                    ( newModel
                    , sendPing welcome
                    )

                WaitingGameScreen playerId self game welcome ->
                    ( newModel
                    , sendPing welcome
                    )

                ActingGameScreen actSelection self game welcome ->
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

        SocketDisconnect ->
            ( { model | connected = False }
            , Cmd.none
            )

        NavigateHome ->
            ( { model
                | ui = WelcomeScreen
                , errors = []
              }
            , navigate model.navKey True HomeRoute
            )

        NavigateHelp ->
            ( model
            , navigate model.navKey True HelpRoute
            )

        NavigateGame welcome ->
            ( { model | ui = RejoinScreen welcome }
            , Cmd.batch
                [ sendPing welcome
                , navigate model.navKey True (GameRoute (getGameCode welcome.gameId) (getPlayerCode welcome.playerId))
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

        InputReorderPlayers players ->
            ( model, Cmd.none )

        SubmitStartGame ->
            case model.ui of
                LobbyScreen players chipsSettings game welcome ->
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
            ( model, Cmd.none )

        Bet amount ->
            ( model, Cmd.none )

        Fold ->
            ( model, Cmd.none )


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


displayFailures : Model -> List Failure -> Model
displayFailures model failures =
    List.foldl displayFailure model failures


parsePersistedGames : Json.Decode.Value -> ( List Welcome, Cmd msg )
parsePersistedGames json =
    let
        persistedGamesResult =
            Json.Decode.decodeValue (Json.Decode.list welcomeDecoder) json
    in
    case persistedGamesResult of
        Ok persistedGames ->
            ( persistedGames, Cmd.none )

        Err decodeError ->
            ( []
            , reportError <|
                errorToString decodeError
            )



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

        LobbyScreen _ _ _ welcome ->
            GameRoute
                (getGameCode welcome.gameId)
                (getPlayerCode welcome.playerId)

        RejoinScreen welcome ->
            GameRoute
                (getGameCode welcome.gameId)
                (getPlayerCode welcome.playerId)

        WaitingGameScreen _ _ _ welcome ->
            GameRoute
                (getGameCode welcome.gameId)
                (getPlayerCode welcome.playerId)

        ActingGameScreen _ _ _ welcome ->
            GameRoute
                (getGameCode welcome.gameId)
                (getPlayerCode welcome.playerId)

        CommunityCardsScreen _ welcome ->
            GameRoute
                (getGameCode welcome.gameId)
                (getPlayerCode welcome.playerId)

        ChipSummaryScreen _ welcome ->
            GameRoute
                (getGameCode welcome.gameId)
                (getPlayerCode welcome.playerId)

        TimerScreen _ _ welcome ->
            GameRoute
                (getGameCode welcome.gameId)
                (getPlayerCode welcome.playerId)


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

        -- etc
        routeParser =
            Url.Parser.oneOf
                [ homeParser
                , gameParser
                , helpParser
                , createParser
                , joinInternalParser
                , joinExternalParser
                ]
    in
    Url.Parser.parse routeParser url
        |> Maybe.withDefault NotFound


followRoute : Browser.Navigation.Key -> Route -> List Welcome -> ( UI, Cmd Msg )
followRoute key route library =
    case route of
        HomeRoute ->
            ( WelcomeScreen
            , Cmd.none
            )

        HelpRoute ->
            ( HelpScreen
            , Cmd.none
            )

        CreateRoute ->
            ( CreateGameScreen "" ""
            , Cmd.none
            )

        JoinRoute Nothing ->
            ( JoinGameScreen True "" ""
            , Cmd.none
            )

        JoinRoute (Just gameCode) ->
            ( JoinGameScreen True gameCode ""
            , Cmd.none
            )

        GameRoute gameCode playerCode ->
            -- check for matching welcome in library
            List.Extra.find
                (\welcome ->
                    String.startsWith gameCode (getGameCode welcome.gameId)
                        && String.startsWith playerCode (getPlayerCode welcome.playerId)
                )
                library
                |> Maybe.map
                    (\welcome ->
                        ( RejoinScreen welcome
                        , sendPing welcome
                        )
                    )
                |> Maybe.withDefault ( WelcomeScreen, navigate key False HomeRoute )

        NotFound ->
            ( WelcomeScreen
            , navigate key False HomeRoute
            )


navigate : Browser.Navigation.Key -> Bool -> Route -> Cmd Msg
navigate navKey withHistory route =
    let
        newUrl =
            case route of
                HomeRoute ->
                    Url.Builder.relative [] []

                HelpRoute ->
                    Url.Builder.relative [ "help" ] []

                CreateRoute ->
                    Url.Builder.relative [ "new" ] []

                JoinRoute (Just gameCode) ->
                    Url.Builder.relative
                        [ "join", gameCode ]
                        []

                JoinRoute Nothing ->
                    Url.Builder.relative
                        [ "join" ]
                        []

                GameRoute gameCode playerCode ->
                    Url.Builder.relative
                        [ "game", gameCode, playerCode ]
                        []

                NotFound ->
                    Url.Builder.relative [] []
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
