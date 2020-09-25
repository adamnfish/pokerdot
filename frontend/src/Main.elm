module Main exposing (..)

import Browser
import Browser.Dom
import Browser.Events
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (src)
import Messages exposing (update)
import Model exposing (..)
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
            , library = []
            }
    in
    ( initial
    , Cmd.batch
        [ Task.perform Tick Time.now
        , Task.perform Resized Browser.Dom.getViewport
        ]
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every 1000 Tick
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
