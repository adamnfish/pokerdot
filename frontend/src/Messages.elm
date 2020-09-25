module Messages exposing (..)

import Model exposing (Model, Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )
