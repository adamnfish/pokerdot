module LogicTest exposing (..)

import Expect
import Logic exposing (..)
import Model exposing (GameId(..), PlayerId(..), Rank(..), Round(..), Suit(..))
import Test exposing (Test, describe, test)
import Time


all : Test
all =
    let
        testPlayer busted =
            { playerId = Pid "player id"
            , screenName = "screen name"
            , isAdmin = False
            , isHost = False
            , stack = 1000
            , pot = 0
            , bet = 0
            , folded = False
            , busted = busted
            , hole = Nothing
            }

        testPlayerWithId busted id =
            { playerId = Pid id
            , screenName = "screen name"
            , isAdmin = False
            , isHost = False
            , stack = 1000
            , pot = 0
            , bet = 0
            , folded = False
            , busted = busted
            , hole = Nothing
            }

        testPlayerWithNoStack busted =
            let
                player =
                    testPlayer busted
            in
            { player | stack = 0 }

        testCard =
            { rank = Queen, suit = Clubs }

        testGame round players =
            { gameId = Gid "game id"
            , gameCode = "1234"
            , gameName = "Test game"
            , players = players
            , spectators = []
            , round = round
            , smallBlind = 10
            , inTurn = Nothing
            , button = 0
            , started = True
            , startTime = Time.millisToPosix 10000
            , trackStacks = True
            , timer = Nothing
            }
    in
    describe "logic"
        [ describe "isBusted"
            [ test "returns true for a busted player" <|
                \_ ->
                    isBusted PreFlopRound (testPlayer True)
                        |> Expect.true "isBusted should return True for a busted player"
            , test "returns False for a player that is not busted" <|
                \_ ->
                    isBusted PreFlopRound (testPlayer False)
                        |> Expect.false "isBusted should return False for a non-busted player"
            , test "returns false for a non-busted player that has an empty stack during a showdown" <|
                -- this is so that the "temporary" showdown status is reflected in the UI
                \_ ->
                    isBusted
                        (ShowdownRound testCard testCard testCard testCard testCard [])
                        (testPlayerWithNoStack False)
                        |> Expect.true "isBusted should return True here, even though the player isn't directly busted"
            ]
        , describe "gameIsFinished"
            [ test "returns False if no players are busted" <|
                \_ ->
                    gameIsFinished
                        (testGame
                            PreFlopRound
                            [ testPlayer False
                            , testPlayer False
                            , testPlayer False
                            ]
                        )
                        |> Expect.equal False
            , test "returns False if multiple players are not busted" <|
                \_ ->
                    gameIsFinished
                        (testGame
                            PreFlopRound
                            [ testPlayer False
                            , testPlayer True
                            , testPlayer False
                            ]
                        )
                        |> Expect.equal False
            , test "returns True if a single player is not busted" <|
                \_ ->
                    gameIsFinished
                        (testGame
                            PreFlopRound
                            [ testPlayer True
                            , testPlayer True
                            , testPlayer False
                            ]
                        )
                        |> Expect.equal True
            , test "returns True if all players are busted" <|
                \_ ->
                    gameIsFinished
                        (testGame
                            PreFlopRound
                            [ testPlayer True
                            , testPlayer True
                            , testPlayer True
                            ]
                        )
                        |> Expect.equal True
            , test "returns True if all players are directly busted, or we are in the showdown and they are only 'busted'" <|
                \_ ->
                    gameIsFinished
                        (testGame
                            (ShowdownRound testCard testCard testCard testCard testCard [])
                            [ testPlayer True
                            , testPlayerWithNoStack False
                            , testPlayer True
                            ]
                        )
                        |> Expect.equal True
            , test "returns True if only on player is not 'busted'" <|
                \_ ->
                    gameIsFinished
                        (testGame
                            (ShowdownRound testCard testCard testCard testCard testCard [])
                            [ testPlayer True
                            , testPlayerWithNoStack False
                            , testPlayer False
                            ]
                        )
                        |> Expect.equal True
            ]
        , describe "winner"
            [ test "returns Nothing if no players are busted" <|
                \_ ->
                    winner
                        (testGame
                            PreFlopRound
                            [ testPlayer False
                            , testPlayer False
                            , testPlayer False
                            ]
                        )
                        |> Expect.equal Nothing
            , test "returns Nothing if multiple players are not busted" <|
                \_ ->
                    winner
                        (testGame
                            PreFlopRound
                            [ testPlayer False
                            , testPlayer True
                            , testPlayer False
                            ]
                        )
                        |> Expect.equal Nothing
            , test "returns the winner if one player is not busted" <|
                \_ ->
                    let
                        winningPlayer =
                            testPlayerWithId False "winner"
                    in
                    winner
                        (testGame
                            PreFlopRound
                            [ testPlayer True
                            , testPlayer True
                            , winningPlayer
                            ]
                        )
                        |> Expect.equal (Just winningPlayer)
            , test "returns Nothing if all players are busted" <|
                \_ ->
                    winner
                        (testGame
                            PreFlopRound
                            [ testPlayer True
                            , testPlayer True
                            , testPlayer True
                            ]
                        )
                        |> Expect.equal Nothing
            , test "returns Nothing if all players are (in)directly busted" <|
                \_ ->
                    winner
                        (testGame
                            (ShowdownRound testCard testCard testCard testCard testCard [])
                            [ testPlayer True
                            , testPlayerWithNoStack False
                            , testPlayer True
                            ]
                        )
                        |> Expect.equal Nothing
            , test "returns the winner if all only they are not (in)directly busted" <|
                \_ ->
                    let
                        winningPlayer =
                            testPlayerWithId False "winner"
                    in
                    winner
                        (testGame
                            (ShowdownRound testCard testCard testCard testCard testCard [])
                            [ testPlayer True
                            , testPlayerWithNoStack False
                            , winningPlayer
                            ]
                        )
                        |> Expect.equal (Just winningPlayer)
            ]
        ]
