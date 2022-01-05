port module Ports exposing (..)

import Json.Encode


port receiveMessage : (Json.Encode.Value -> msg) -> Sub msg


port sendMessage : Json.Encode.Value -> Cmd msg


port socketConnect : (Maybe Int -> msg) -> Sub msg


port socketDisconnect : (Maybe Int -> msg) -> Sub msg


port blurs : (() -> msg) -> Sub msg


port scrollToTop : () -> Cmd msg


port reportError : String -> Cmd msg


port persistNewGame : Json.Encode.Value -> Cmd msg


port deletePersistedGame : Json.Encode.Value -> Cmd msg


port requestPersistedGames : () -> Cmd msg


port receivePersistedGames : (Json.Encode.Value -> msg) -> Sub msg
