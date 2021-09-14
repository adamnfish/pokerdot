module UtilsTest exposing (..)

import Expect
import Fuzz exposing (intRange)
import Test exposing (Test, describe, fuzz, test)
import Utils exposing (millisToTimeComponents, swapDown, swapUp)


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
        , describe "millisToTimeComponents"
            [ fuzz (intRange 0 999) "just millis for a small number" <|
                \n ->
                    millisToTimeComponents n
                        |> Expect.equal
                            { millis = n
                            , seconds = 0
                            , minutes = 0
                            , hours = 0
                            , days = 0
                            , weeks = 0
                            }
            , fuzz (intRange 1000 1999) "millis and a second" <|
                \n ->
                    millisToTimeComponents n
                        |> Expect.equal
                            { millis = n - 1000
                            , seconds = 1
                            , minutes = 0
                            , hours = 0
                            , days = 0
                            , weeks = 0
                            }
            , fuzz (intRange 0 59) "minutes" <|
                \n ->
                    millisToTimeComponents (1000 * 60 * n)
                        |> Expect.equal
                            { millis = 0
                            , seconds = 0
                            , minutes = n
                            , hours = 0
                            , days = 0
                            , weeks = 0
                            }
            , fuzz (intRange 0 23) "hours" <|
                \n ->
                    millisToTimeComponents (1000 * 60 * 60 * n)
                        |> Expect.equal
                            { millis = 0
                            , seconds = 0
                            , minutes = 0
                            , hours = n
                            , days = 0
                            , weeks = 0
                            }
            , fuzz (intRange 0 6) "days" <|
                \n ->
                    millisToTimeComponents (86400 * 1000 * n)
                        |> Expect.equal
                            { millis = 0
                            , seconds = 0
                            , minutes = 0
                            , hours = 0
                            , days = n
                            , weeks = 0
                            }
            , fuzz (intRange 0 10) "weeks" <|
                \n ->
                    millisToTimeComponents (86400 * 7 * 1000 * n)
                        |> Expect.equal
                            { millis = 0
                            , seconds = 0
                            , minutes = 0
                            , hours = 0
                            , days = 0
                            , weeks = n
                            }
            ]
        ]
