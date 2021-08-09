module UtilsTest exposing (..)

import Expect
import Test exposing (Test, describe, test)
import Utils exposing (swapDown, swapUp)


all : Test
all =
    describe "utils"
        [ describe "swapDown"
            [ test "swaps the matching element 'down'" <|
                \_ ->
                    swapDown 2 [ 1, 2, 3 ] |> Expect.equal [ 1, 3, 2 ]
            , test "doesn't swap the element down if it is at the back already" <|
                \_ ->
                    swapDown 3 [ 1, 2, 3 ] |> Expect.equal [ 1, 2, 3 ]
            , test "swaps front element down" <|
                \_ ->
                    swapDown 1 [ 1, 2, 3 ] |> Expect.equal [ 2, 1, 3 ]
            , test "keeps existing list if el does not exist" <|
                \_ ->
                    swapDown 4 [ 1, 2, 3 ] |> Expect.equal [ 1, 2, 3 ]
            ]
        , describe "swapUp"
            [ test "swaps the matching element 'up'" <|
                \_ ->
                    swapUp 2 [ 1, 2, 3 ] |> Expect.equal [ 2, 1, 3 ]
            , test "doesn't swap the element up if it is at the front already" <|
                \_ ->
                    swapUp 1 [ 1, 2, 3 ] |> Expect.equal [ 1, 2, 3 ]
            , test "swaps back element up" <|
                \_ ->
                    swapUp 3 [ 1, 2, 3 ] |> Expect.equal [ 1, 3, 2 ]
            , test "keeps existing list if el does not exist" <|
                \_ ->
                    swapUp 4 [ 1, 2, 3 ] |> Expect.equal [ 1, 2, 3 ]
            ]
        ]
