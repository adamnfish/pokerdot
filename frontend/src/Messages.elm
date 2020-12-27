module Messages exposing (..)

import Browser.Dom
import Json.Decode exposing (errorToString)
import List.Extra
import Model exposing (CreateGameRequest, Failure, JoinGameRequest, LoadingStatus(..), Message(..), Model, Msg(..), PingRequest, UI(..), Welcome, createGameRequestEncoder, getGameCode, joinGameRequestEncoder, messageDecoder, pingRequestEncoder, wakeRequestEncoder, welcomeDecoder, welcomeEncoder)
import Ports exposing (deletePersistedGame, persistNewGame, reportError, requestPersistedGames, sendMessage)
import Task
import Time


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
            ( model, Cmd.none )

        ServerMessage json ->
            let
                parsedMessage =
                    Json.Decode.decodeValue messageDecoder json
            in
            case parsedMessage of
                Ok (WelcomeMessage welcome) ->
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
                                    , ui = LobbyScreen [] Nothing welcome
                                  }
                                , Cmd.none
                                )

                            else
                                -- different game, background update
                                ( { model | library = newLibrary }
                                , Cmd.none
                                )

                        JoinGameScreen gameCode _ ->
                            if gameCode == getGameCode welcome.gameId then
                                ( { model
                                    | library = newLibrary
                                    , ui = LobbyScreen [] Nothing welcome
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

                Ok (PlayerRoundWinningsMessage self game results) ->
                    ( model, Cmd.none )

                Ok (SpectatorGameStatusMessage spectator game action) ->
                    ( model, Cmd.none )

                Ok (SpectatorRoundWinningsMessage spectator game results) ->
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

                JoinGameScreen gameCode screenName ->
                    ( newModel, Cmd.none )

                LobbyScreen players maybe welcome ->
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
            , Cmd.none
            )

        NavigateHelp ->
            ( model, Cmd.none )

        NavigateGame welcome ->
            ( { model | ui = RejoinScreen welcome }
            , sendPing welcome
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
            , Cmd.none
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
            ( { model | ui = JoinGameScreen "" "" }
            , Cmd.none
            )

        InputJoinGame gameCode screenName ->
            ( { model | ui = JoinGameScreen gameCode screenName }
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
            ( model, Cmd.none )

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



-- server requests


sendCreateGame : CreateGameRequest -> Cmd Msg
sendCreateGame createGameRequest =
    sendMessage <| createGameRequestEncoder createGameRequest


sendJoinGame : JoinGameRequest -> Cmd Msg
sendJoinGame joinGameRequest =
    sendMessage <| joinGameRequestEncoder joinGameRequest


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
