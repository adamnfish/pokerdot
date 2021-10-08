module Utils exposing (..)

import List.Extra


flip : (a -> b -> c) -> (b -> a -> c)
flip fn b a =
    fn a b


swapDown : x -> List x -> List x
swapDown x xs =
    Maybe.withDefault xs <|
        Maybe.map
            (\i -> List.Extra.swapAt i (i + 1) xs)
        <|
            List.Extra.elemIndex x xs


swapUp : x -> List x -> List x
swapUp x xs =
    Maybe.withDefault xs <|
        Maybe.map
            (\i -> List.Extra.swapAt i (i - 1) xs)
        <|
            List.Extra.elemIndex x xs


type alias TimeComponents =
    { millis : Int
    , seconds : Int
    , minutes : Int
    , hours : Int
    , days : Int
    , weeks : Int
    }


millisToTimeComponents : Int -> TimeComponents
millisToTimeComponents totalMillis =
    let
        millis =
            modBy 1000 totalMillis

        seconds =
            modBy 60 <| floor (toFloat totalMillis / 1000)

        minutes =
            modBy 60 <| floor (toFloat totalMillis / 1000 / 60)

        hours =
            modBy 24 <| floor (toFloat totalMillis / 1000 / 60 / 60)

        days =
            modBy 7 <| floor (toFloat totalMillis / 1000 / 60 / 60 / 24)

        weeks =
            floor (toFloat totalMillis / 1000 / 60 / 60 / 24 / 7)
    in
    { millis = millis
    , seconds = seconds
    , minutes = minutes
    , hours = hours
    , days = days
    , weeks = weeks
    }
