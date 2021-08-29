module MessageDecodersTest exposing (..)

import Expect exposing (fail)
import Json.Decode
import Model exposing (Action(..), GameId(..), Message(..), PlayerId(..), PlayerKey(..), messageDecoder, playerGameStatusMessageDecoder)
import Test exposing (..)


messageDecoders : Test
messageDecoders =
    describe "message decoders"
        [ describe "Failure messages"
            [ test "parses an example" <|
                \_ ->
                    let
                        failureMessageJson =
                            """{"failures":[{"message":"game name is required","context":"game name"},{"message":"screen name is required","context":"screen name"},{"message":"Another message","context":null}]}"""

                        result =
                            Json.Decode.decodeString messageDecoder failureMessageJson
                    in
                    case result of
                        Ok (FailureMessage failures) ->
                            Expect.equal failures
                                [ { message = "game name is required"
                                  , context = Just "game name"
                                  }
                                , { message = "screen name is required"
                                  , context = Just "screen name"
                                  }
                                , { message = "Another message"
                                  , context = Nothing
                                  }
                                ]

                        Ok message ->
                            fail ("Expected Failure message, got " ++ Debug.toString message)

                        Err err ->
                            fail <| Json.Decode.errorToString err
            , test "parses another example" <|
                \_ ->
                    let
                        failureMessageJson =
                            """{"failures":[{"message":"couldn't find game, is the code correct?"}]}"""

                        result =
                            Json.Decode.decodeString messageDecoder failureMessageJson
                    in
                    case result of
                        Ok (FailureMessage failures) ->
                            Expect.equal failures
                                [ { message = "couldn't find game, is the code correct?"
                                  , context = Nothing
                                  }
                                ]

                        Ok message ->
                            fail ("Expected Failure message, got " ++ Debug.toString message)

                        Err err ->
                            fail <| Json.Decode.errorToString err
            ]
        , describe "welcome messages"
            [ test "parses an example" <|
                \_ ->
                    let
                        welcomeMessageJson =
                            """{"playerKey":"b4043f56-a225-4b21-bf49-797503b1f035","playerId":"3f37c05f-9bd6-4708-b989-3896f480180f","gameId":"6945a740-15f4-4409-b60f-3490e7674cbe","gameCode":"6945","gameName":"Test","screenName":"Player 1","spectator":false,"game":{"gameId":"6945a740-15f4-4409-b60f-3490e7674cbe","gameCode":"6945","gameName":"Test","players":[{"playerId":"3f37c05f-9bd6-4708-b989-3896f480180f","screenName":"Player 1","stack":0,"pot":0,"bet":0,"folded":false,"busted":false,"isHost":false,"isAdmin":false,"hole":null},{"playerId":"de855f82-bc5d-47db-adc2-0040853674ee","screenName":"host","stack":0,"pot":0,"bet":0,"folded":false,"busted":false,"isHost":true,"isAdmin":true,"hole":null}],"spectators":[],"round":{"phase":"pre-flop"},"smallBlind":0,"inTurn":null,"button":0,"started":false,"startTime":1613414159747,"trackStacks":false,"timer":null},"self":{"playerId":"3f37c05f-9bd6-4708-b989-3896f480180f","screenName":"Player 1","stack":0,"pot":0,"bet":0,"folded":false,"busted":false,"hole":null,"isHost":false,"isAdmin":false}}"""

                        result =
                            Json.Decode.decodeString messageDecoder welcomeMessageJson
                    in
                    case result of
                        Ok (WelcomeMessage welcome self game) ->
                            Expect.equal welcome
                                { playerKey = Pkey "b4043f56-a225-4b21-bf49-797503b1f035"
                                , playerId = Pid "3f37c05f-9bd6-4708-b989-3896f480180f"
                                , gameId = Gid "6945a740-15f4-4409-b60f-3490e7674cbe"
                                , gameCode = "6945"
                                , gameName = "Test"
                                , screenName = "Player 1"
                                , spectator = False
                                }

                        Ok message ->
                            fail ("Expected Welcome message, got " ++ Debug.toString message)

                        Err err ->
                            fail <| Json.Decode.errorToString err
            ]
        , describe "game status messages"
            [ test "parses player-joined message" <|
                \_ ->
                    let
                        playerJoinedMessage =
                            """{"self":{"playerId":"de855f82-bc5d-47db-adc2-0040853674ee","screenName":"host","stack":0,"pot":0,"bet":0,"folded":false,"busted":false,"hole":null,"isHost":true,"isAdmin":true},"game":{"gameId":"6945a740-15f4-4409-b60f-3490e7674cbe","gameCode":"6945","gameName":"Test","players":[{"playerId":"3f37c05f-9bd6-4708-b989-3896f480180f","screenName":"Player 1","stack":0,"pot":0,"bet":0,"folded":false,"busted":false,"isHost":false,"isAdmin":false,"hole":null},{"playerId":"de855f82-bc5d-47db-adc2-0040853674ee","screenName":"host","stack":0,"pot":0,"bet":0,"folded":false,"busted":false,"isHost":true,"isAdmin":true,"hole":null}],"spectators":[],"round":{"phase":"pre-flop"},"smallBlind":0,"inTurn":null,"button":0,"started":false,"startTime":1613414159747,"trackStacks":false,"timer":null},"action":{"playerId":"3f37c05f-9bd6-4708-b989-3896f480180f","action":"player-joined"}}"""

                        result =
                            Json.Decode.decodeString messageDecoder playerJoinedMessage
                    in
                    case result of
                        Ok (PlayerGameStatusMessage self game action) ->
                            Expect.equal action <| PlayerJoinedAction (Pid "3f37c05f-9bd6-4708-b989-3896f480180f")

                        Ok message ->
                            fail ("Expected game status message, got " ++ Debug.toString message)

                        Err err ->
                            fail <| Json.Decode.errorToString err
            ]
        ]
