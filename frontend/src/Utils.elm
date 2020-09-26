module Utils exposing (..)


flip : (a -> b -> c) -> (b -> a -> c)
flip fn b a =
    fn a b
