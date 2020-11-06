module MessageDecodersTest exposing (..)

import Expect exposing (fail)
import Json.Decode
import Model exposing (GameId(..), Message(..), PlayerId(..), PlayerKey(..), messageDecoder, playerGameStatusMessageDecoder)
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
                            """{"failures":[{"message":"Couldn't find game, is the code correct?"}]}"""

                        result =
                            Json.Decode.decodeString messageDecoder failureMessageJson
                    in
                    case result of
                        Ok (FailureMessage failures) ->
                            Expect.equal failures
                                [ { message = "Couldn't find game, is the code correct?"
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
                            """{"playerKey":"a0968cf4-c799-4632-948f-81a51e8aac76","playerId":"101d501d-ce6f-455c-9413-03bb260b2f49","gameId":"903053a3-3fd1-4016-aa48-52386c554a43","gameName":"game name","screenName":"screen name","spectator":false}"""

                        result =
                            Json.Decode.decodeString messageDecoder welcomeMessageJson
                    in
                    case result of
                        Ok (WelcomeMessage welcome) ->
                            Expect.equal welcome
                                { playerKey = Pkey "a0968cf4-c799-4632-948f-81a51e8aac76"
                                , playerId = Pid "101d501d-ce6f-455c-9413-03bb260b2f49"
                                , gameId = Gid "903053a3-3fd1-4016-aa48-52386c554a43"
                                , gameName = "game name"
                                , screenName = "screen name"
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
                            """{"self":{"playerId":"17aa928d-4c56-4928-bbd0-d7593ab5ccf9","screenName":"2","stack":0,"pot":0,"bid":0,"folded":false,"busted":false,"hole":null},"game":{"gameId":"5df0043c-b2ec-4285-b5aa-17671ff0442a","gameName":"asdasd","players":[{"playerId":"17aa928d-4c56-4928-bbd0-d7593ab5ccf9","screenName":"2","stack":0,"pot":0,"bid":0,"folded":false,"busted":false},{"playerId":"8b555d73-0d59-471c-9e8c-90e6a1ecf41a","screenName":"1","stack":0,"pot":0,"bid":0,"folded":false,"busted":false}],"spectators":[],"round":{},"inTurn":null,"button":0,"started":false,"startTime":1601224730,"trackStacks":false,"timer":null},"action":{"player":{"playerId":"17aa928d-4c56-4928-bbd0-d7593ab5ccf9","screenName":"2","stack":0,"pot":0,"bid":0,"folded":false,"busted":false},"action":"player-joined"}}"""

                        result =
                            Json.Decode.decodeString messageDecoder playerJoinedMessage
                    in
                    case result of
                        Ok (PlayerGameStatusMessage self game action) ->
                            -- TODO: check the actual values!
                            Expect.equal 1 1

                        Ok message ->
                            fail ("Expected game status message, got " ++ Debug.toString message)

                        Err err ->
                            fail <| Json.Decode.errorToString err
            ]
        ]
