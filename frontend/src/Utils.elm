module Utils exposing (..)

import List.Extra


flip : (a -> b -> c) -> (b -> a -> c)
flip fn b a =
    fn a b


maybeContains : a -> Maybe a -> Bool
maybeContains a ma =
    case ma of
        Nothing ->
            False

        Just am ->
            am == a


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
