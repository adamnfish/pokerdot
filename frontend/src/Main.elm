module Main exposing (..)

import Browser
import Browser.Dom
import Browser.Events
import Browser.Navigation
import Messages exposing (followRoute, routeFromUrl, sendWake, update)
import Model exposing (..)
import Ports exposing (receiveMessage, socketConnect, socketDisconnect)
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

        -- TODO: load from flags so it's available for the navigation check
        initialLibrary =
            []

        ( ui, navCmd ) =
            followRoute navKey initialRoute initialLibrary

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
            , library = initialLibrary
            , url = url
            , navKey = navKey
            }
    in
    ( initial
    , Cmd.batch
        [ Task.perform Tick Time.now
        , Task.perform Resized Browser.Dom.getViewport
        , sendWake ()
        , navCmd
        ]
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ receiveMessage ServerMessage
        , socketConnect <| always SocketConnect
        , socketDisconnect <| always SocketDisconnect
        , Time.every 1000 Tick
        , Browser.Events.onResize (\_ _ -> OnResize)
        ]


type alias Flags =
    ()



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
