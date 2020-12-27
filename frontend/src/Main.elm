module Main exposing (..)

import Browser
import Browser.Dom
import Browser.Events
import Browser.Navigation
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (src)
import Messages exposing (sendWake, update)
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
        initial : Model
        initial =
            { ui = WelcomeScreen
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
                    , width = 360
                    , height = 640
                    }
                }
            , peeking = False
            , loadingStatus = NotLoading
            , errors = []
            , library = []
            , url = url
            , navKey = navKey
            }
    in
    ( initial
    , Cmd.batch
        [ Task.perform Tick Time.now
        , Task.perform Resized Browser.Dom.getViewport
        , sendWake ()
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


type alias Flags = ()


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
