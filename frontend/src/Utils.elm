module Utils exposing (..)


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
