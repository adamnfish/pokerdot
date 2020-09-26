module Main exposing (..)

import Browser
import Browser.Dom
import Browser.Events
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (src)
import Messages exposing (sendWake, update)
import Model exposing (..)
import Ports exposing (receiveMessage, socketConnect, socketDisconnect)
import Task
import Time
import Views.UI exposing (view)



---- MODEL ----


init : ( Model, Cmd Msg )
init =
    let
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



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.document
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
