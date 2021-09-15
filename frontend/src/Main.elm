module Main exposing (..)

import Browser
import Browser.Dom
import Browser.Events
import Browser.Navigation
import Json.Encode
import Messages exposing (parsePersistedGames, routeFromUrl, sendPing, sendWake, uiFromRoute, update)
import Model exposing (..)
import Ports exposing (receiveMessage, receivePersistedGames, requestPersistedGames, socketConnect, socketDisconnect)
import Task
import Time
import Url
import Views.UI exposing (view)



---- MODEL ----


init : Flags -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        initialRoute =
            routeFromUrl url

        ( initialLibrary, parseLibraryCmd ) =
            parsePersistedGames flags.library

        ui =
            uiFromRoute initialRoute initialLibrary

        rejoinCmd =
            case ui of
                RejoinScreen welcome ->
                    sendPing welcome

                _ ->
                    Cmd.none

        initial : Model
        initial =
            { ui = ui
            , connected = False
            , now = Time.millisToPosix 0
            , viewport =
                { scene =
                    { width = 0
                    , height = 0
                    }
                , viewport =
                    { x = 0
                    , y = 0

                    -- TODO: from flags
                    , width = 360
                    , height = 640
                    }
                }
            , peeking = False
            , loadingStatus = NotLoading
            , errors = []
            , events = []
            , library = initialLibrary
            , navKey = navKey
            }
    in
    ( initial
    , Cmd.batch
        [ Task.perform Tick Time.now
        , Task.perform Resized Browser.Dom.getViewport
        , sendWake ()
        , rejoinCmd
        , requestPersistedGames ()
        , parseLibraryCmd
        ]
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        noTickRequired =
            List.isEmpty model.errors && List.isEmpty model.events
    in
    Sub.batch
        [ receiveMessage ServerMessage
        , socketConnect <| always SocketConnect
        , socketDisconnect <| always SocketDisconnect
        , Time.every 1000 Tick
        , Browser.Events.onResize (\_ _ -> OnResize)
        , receivePersistedGames UpdateLibrary
        ]


type alias Flags =
    { library : Json.Encode.Value
    }



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.application
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChange
        , onUrlRequest = UrlRequest
        }
