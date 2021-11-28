module Model exposing (..)

import Browser exposing (UrlRequest)
import Browser.Dom exposing (Viewport)
import Browser.Navigation
import Json.Decode exposing (nullable)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode
import Time exposing (Posix(..), millisToPosix, posixToMillis)
import Url


type Msg
    = NoOp
    | Tick Time.Posix
    | OnResize
    | Resized Viewport
      -- URLs
    | UrlChange Url.Url
    | UrlRequest UrlRequest
      -- connections
    | ServerMessage Json.Encode.Value
    | SocketConnect
    | SocketDisconnect
      -- basic navigation
    | NavigateHome
    | NavigateHelp
    | NavigateGame Welcome
      -- library management
    | UpdateLibrary Json.Encode.Value
    | PersistGame Welcome
    | DeletePersistedGame Welcome
    | RequestPersistedGames
      -- create game
    | NavigateCreateGame
    | InputCreateGame String String
    | SubmitCreateGame String String
      -- join game
    | NavigateJoinGame
    | InputJoinGame Bool String String
    | SubmitJoinGame String String
      -- lobby
    | InputStartGameSettings (List Player) ChipsSettings
    | SubmitStartGame
      -- game messages
    | TogglePeek
      -- action selection events
    | InputActSelection ActSelection
    | Check
    | InputBet Int
    | Bet Int
    | Fold
    | AdvancePhase
      -- blind management
    | InputUpdateBlind EditBlindsSettings
    | UpdateBlind EditBlindsSettings
      -- debugging / development
    | NavigateUIElements Int


type alias Model =
    { ui : UI
    , connected : Bool
    , now : Time.Posix
    , viewport : Viewport
    , peeking : Bool -- TODO: should this be in the UI?
    , loadingStatus : LoadingStatus
    , errors : List Error
    , events : List Event
    , library : List Welcome
    , navKey : Browser.Navigation.Key
    }


type UI
    = WelcomeScreen
    | HelpScreen
    | CreateGameScreen String String
    | JoinGameScreen Bool String String
    | LobbyScreen (List Player) ChipsSettings Self Game Welcome
    | RejoinScreen Welcome
    | GameScreen ActSelection Self Game Welcome
    | RoundResultScreen (List PotResult) (List PlayerWinnings) Self Game Welcome EditBlindsSettings
      -- TODO: admin screen to fix things
      -- spectating
    | CommunityCardsScreen Game Welcome
    | TimerScreen TimerStatus Game Welcome
    | ChipSummaryScreen Game Welcome
      -- debugging / development
    | UIElementsScreen Int ActSelection


type Route
    = HomeRoute
    | HelpRoute
    | CreateRoute
    | JoinRoute (Maybe String)
    | GameRoute String String
      -- TODO: routes for spectating screens?
    | UiElementsRoute
    | NotFound


type LoadingStatus
    = NotLoading
    | AwaitingMessage



-- TODO: consider multiple pending messages / background loading
--| BackgroundMessage


type ChipsSettings
    = DoNotTrackChips
    | TrackWithTimer Int (List TimerLevel)
    | TrackWithManualBlinds Int Int


type EditBlindsSettings
    = DoNotEditBlinds
    | DoNotTrackBlinds
    | ManualBlinds Int
    | TimerBlinds (List TimerLevel)


defaultChipSettings =
    TrackWithManualBlinds 1000 5


type alias Error =
    { failure : Failure
    , time : Time.Posix
    }


type alias Failure =
    -- TODO: include level here (at least error vs info)
    { message : String
    , context : Maybe String
    }


type GameId
    = Gid String


type PlayerId
    = Pid String


type PlayerKey
    = Pkey String


getPlayerCode : PlayerId -> String
getPlayerCode (Pid pid) =
    String.left 4 pid


type alias Player =
    { playerId : PlayerId
    , screenName : String
    , isAdmin : Bool
    , isHost : Bool
    , stack : Int
    , pot : Int
    , bet : Int
    , folded : Bool
    , busted : Bool
    , hole : Maybe ( Card, Card )
    }


type alias Spectator =
    { playerId : PlayerId
    , screenName : String
    , isAdmin : Bool
    , isHost : Bool
    }


type alias Welcome =
    { playerKey : PlayerKey
    , playerId : PlayerId
    , gameId : GameId
    , gameCode : String
    , gameName : String
    , screenName : String
    , spectator : Bool
    , joined : Posix
    }


type alias Self =
    { playerId : PlayerId
    , screenName : String
    , isAdmin : Bool
    , isHost : Bool
    , stack : Int
    , pot : Int
    , bet : Int
    , folded : Bool
    , busted : Bool
    , hole : Maybe ( Card, Card )
    }


type alias Game =
    { gameId : GameId
    , gameCode : String
    , gameName : String
    , players : List Player
    , spectators : List Spectator
    , round : Round
    , smallBlind : Int
    , inTurn : Maybe PlayerId
    , button : Int
    , started : Bool
    , startTime : Time.Posix
    , trackStacks : Bool
    , timer : Maybe TimerStatus
    }


type alias TimerStatus =
    { timerStartTime : Time.Posix
    , pausedTime : Maybe Time.Posix
    , levels : List TimerLevel
    }


type TimerLevel
    = RoundLevel Int Int
    | BreakLevel Int


type Round
    = PreFlopRound
    | FlopRound Card Card Card
    | TurnRound Card Card Card Card
    | RiverRound Card Card Card Card Card
    | ShowdownRound Card Card Card Card Card (List ( PlayerId, Card, Card ))


type Action
    = GameStartedAction
    | PlayerJoinedAction PlayerId
    | CallAction PlayerId
    | BetAction PlayerId Int
    | CheckAction PlayerId
    | FoldAction PlayerId
    | AdvancePhaseAction
    | TimerStatusAction Bool
    | EditTimerAction
    | EditBlindAction
    | NoAction


type alias Event =
    { action : Action
    , time : Time.Posix
    }


type alias PotResult =
    { potSize : Int
    , participants : List PlayerId
    , winners : List PlayerId
    }


type alias PlayerWinnings =
    { playerId : PlayerId
    , hand : Maybe Hand
    , hole : ( Card, Card )
    , winnings : Int
    }



-- UI states


type ActSelection
    = ActBet Int
    | ActCheck
    | ActCall
    | ActFold
    | NoAct


type PlayingState
    = Playing
    | Waiting
    | Idle



-- Poker


type alias Card =
    { rank : Rank
    , suit : Suit
    }


type Rank
    = Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King
    | Ace


type Suit
    = Clubs
    | Diamonds
    | Spades
    | Hearts


type Hand
    = HighCard Card Card Card Card Card
    | Pair Card Card Card Card Card
    | TwoPair Card Card Card Card Card
    | ThreeOfAKind Card Card Card Card Card
    | Straight Card Card Card Card Card
    | Flush Card Card Card Card Card
    | FullHouse Card Card Card Card Card
    | FourOfAKind Card Card Card Card Card
    | StraightFlush Card Card Card Card Card



-- Server messages


type Message
    = WelcomeMessage Welcome Self Game
    | PlayerGameStatusMessage Self Game Action
    | SpectatorGameStatusMessage Spectator Game Action
    | PlayerRoundWinningsMessage Self Game (List PotResult) (List PlayerWinnings)
    | SpectatorRoundWinningsMessage Spectator Game (List PotResult) (List PlayerWinnings)
    | StatusMessage String
    | FailureMessage (List Failure)



-- Requests


type alias CreateGameRequest =
    { screenName : String
    , gameName : String
    }


type alias JoinGameRequest =
    { gameCode : String
    , screenName : String
    }


type alias StartGameRequest =
    { gameId : GameId
    , playerId : PlayerId
    , playerKey : PlayerKey
    , startingStack : Maybe Int
    , initialSmallBlind : Maybe Int
    , timerConfig : Maybe (List TimerLevel)
    , playerOrder : List PlayerId
    }


type alias BetRequest =
    { gameId : GameId
    , playerKey : PlayerKey
    , playerId : PlayerId
    , betAmount : Int
    }


type alias CheckRequest =
    { gameId : GameId
    , playerKey : PlayerKey
    , playerId : PlayerId
    }


type alias FoldRequest =
    { gameId : GameId
    , playerKey : PlayerKey
    , playerId : PlayerId
    }


type alias AdvancePhaseRequest =
    { gameId : GameId
    , playerKey : PlayerKey
    , playerId : PlayerId
    }


type alias PingRequest =
    { gameId : GameId
    , playerId : PlayerId
    , playerKey : PlayerKey
    }


type alias UpdateBlindRequest =
    { gameId : GameId
    , playerId : PlayerId
    , playerKey : PlayerKey
    , timerLevels : Maybe (List TimerLevel)
    , progress : Maybe Int
    , smallBlind : Maybe Int
    , playing : Maybe Bool
    }



-- Codecs


persistedWelcomeDecoder : Json.Decode.Decoder Welcome
persistedWelcomeDecoder =
    Json.Decode.succeed Welcome
        |> required "playerKey" playerKeyDecoder
        |> required "playerId" playerIdDecoder
        |> required "gameId" gameIdDecoder
        |> required "gameCode" Json.Decode.string
        |> required "gameName" Json.Decode.string
        |> required "screenName" Json.Decode.string
        |> required "spectator" Json.Decode.bool
        |> required "joined" (Json.Decode.map millisToPosix Json.Decode.int)


serverWelcomeDecoder : Posix -> Json.Decode.Decoder Welcome
serverWelcomeDecoder now =
    Json.Decode.succeed Welcome
        |> required "playerKey" playerKeyDecoder
        |> required "playerId" playerIdDecoder
        |> required "gameId" gameIdDecoder
        |> required "gameCode" Json.Decode.string
        |> required "gameName" Json.Decode.string
        |> required "screenName" Json.Decode.string
        |> required "spectator" Json.Decode.bool
        |> hardcoded now


selfDecoder : Json.Decode.Decoder Self
selfDecoder =
    Json.Decode.succeed Self
        |> required "playerId" playerIdDecoder
        |> required "screenName" Json.Decode.string
        |> required "isAdmin" Json.Decode.bool
        |> required "isHost" Json.Decode.bool
        |> required "stack" Json.Decode.int
        |> required "pot" Json.Decode.int
        |> required "bet" Json.Decode.int
        |> required "folded" Json.Decode.bool
        |> required "busted" Json.Decode.bool
        |> required "hole" (Json.Decode.nullable holeDecoder)


spectatorDecoder : Json.Decode.Decoder Spectator
spectatorDecoder =
    Json.Decode.succeed Spectator
        |> required "playerId" playerIdDecoder
        |> required "screenName" Json.Decode.string
        |> required "isAdmin" Json.Decode.bool
        |> required "isHost" Json.Decode.bool


gameDecoder : Json.Decode.Decoder Game
gameDecoder =
    Json.Decode.succeed Game
        |> required "gameId" gameIdDecoder
        |> required "gameCode" Json.Decode.string
        |> required "gameName" Json.Decode.string
        |> required "players" (Json.Decode.list playerDecoder)
        |> required "spectators" (Json.Decode.list spectatorDecoder)
        |> required "round" roundDecoder
        |> required "smallBlind" Json.Decode.int
        |> required "inTurn" (Json.Decode.nullable playerIdDecoder)
        |> required "button" Json.Decode.int
        |> required "started" Json.Decode.bool
        |> required "startTime" posixDecoder
        |> required "trackStacks" Json.Decode.bool
        |> required "timer" (Json.Decode.nullable timerStatusDecoder)


actionDecoder : Json.Decode.Decoder Action
actionDecoder =
    let
        playerIdFieldDecoder =
            Json.Decode.field "playerId" playerIdDecoder

        playingStatusFieldDecoder =
            Json.Decode.field "playing" Json.Decode.bool

        decode id =
            case id of
                "game-started" ->
                    Json.Decode.succeed GameStartedAction

                "player-joined" ->
                    playerIdFieldDecoder
                        |> Json.Decode.map PlayerJoinedAction

                "call" ->
                    playerIdFieldDecoder
                        |> Json.Decode.map CallAction

                "bet" ->
                    Json.Decode.map2 BetAction
                        playerIdFieldDecoder
                        (Json.Decode.field "bet" Json.Decode.int)

                "check" ->
                    playerIdFieldDecoder
                        |> Json.Decode.map CheckAction

                "fold" ->
                    playerIdFieldDecoder
                        |> Json.Decode.map FoldAction

                "advance-phase" ->
                    Json.Decode.succeed AdvancePhaseAction

                "timer-status" ->
                    playingStatusFieldDecoder
                        |> Json.Decode.map TimerStatusAction

                "edit-timer" ->
                    Json.Decode.succeed EditTimerAction

                "edit-blind" ->
                    Json.Decode.succeed EditBlindAction

                "no-action" ->
                    Json.Decode.succeed NoAction

                _ ->
                    Json.Decode.fail "Couldn't understand the server's description of the action that was performed."
    in
    Json.Decode.field "action" Json.Decode.string
        |> Json.Decode.andThen decode


playerDecoder : Json.Decode.Decoder Player
playerDecoder =
    Json.Decode.succeed Player
        |> required "playerId" playerIdDecoder
        |> required "screenName" Json.Decode.string
        |> required "isAdmin" Json.Decode.bool
        |> required "isHost" Json.Decode.bool
        |> required "stack" Json.Decode.int
        |> required "pot" Json.Decode.int
        |> required "bet" Json.Decode.int
        |> required "folded" Json.Decode.bool
        |> required "busted" Json.Decode.bool
        |> required "hole" (Json.Decode.nullable holeDecoder)


roundDecoder : Json.Decode.Decoder Round
roundDecoder =
    let
        playerHoleDecoder =
            Json.Decode.map2
                (\playerId ( card1, card2 ) -> ( playerId, card1, card2 ))
                (Json.Decode.index 0 playerIdDecoder)
                (Json.Decode.index 1 holeDecoder)

        preFlopDecoder =
            Json.Decode.succeed PreFlopRound

        flopDecoder =
            Json.Decode.succeed FlopRound
                |> required "flop1" cardDecoder
                |> required "flop2" cardDecoder
                |> required "flop3" cardDecoder

        turnDecoder =
            Json.Decode.succeed TurnRound
                |> required "flop1" cardDecoder
                |> required "flop2" cardDecoder
                |> required "flop3" cardDecoder
                |> required "turn" cardDecoder

        riverDecoder =
            Json.Decode.succeed RiverRound
                |> required "flop1" cardDecoder
                |> required "flop2" cardDecoder
                |> required "flop3" cardDecoder
                |> required "turn" cardDecoder
                |> required "river" cardDecoder

        showdownDecoder =
            Json.Decode.succeed ShowdownRound
                |> required "flop1" cardDecoder
                |> required "flop2" cardDecoder
                |> required "flop3" cardDecoder
                |> required "turn" cardDecoder
                |> required "river" cardDecoder
                |> required "holes" (Json.Decode.list playerHoleDecoder)
    in
    Json.Decode.oneOf
        [ showdownDecoder
        , riverDecoder
        , turnDecoder
        , flopDecoder
        , preFlopDecoder
        ]


potResultDecoder : Json.Decode.Decoder PotResult
potResultDecoder =
    Json.Decode.succeed PotResult
        |> required "potSize" Json.Decode.int
        |> required "participants" (Json.Decode.list playerIdDecoder)
        |> required "winners" (Json.Decode.list playerIdDecoder)


playerWinningsDecoder : Json.Decode.Decoder PlayerWinnings
playerWinningsDecoder =
    Json.Decode.succeed PlayerWinnings
        |> required "playerId" playerIdDecoder
        |> required "hand" (nullable handDecoder)
        |> required "hole" holeDecoder
        |> required "winnings" Json.Decode.int


handDecoder : Json.Decode.Decoder Hand
handDecoder =
    let
        decode hand =
            case hand of
                "high-card" ->
                    Json.Decode.map5 HighCard
                        (Json.Decode.field "highCard" cardDecoder)
                        (Json.Decode.field "kicker1" cardDecoder)
                        (Json.Decode.field "kicker2" cardDecoder)
                        (Json.Decode.field "kicker3" cardDecoder)
                        (Json.Decode.field "kicker4" cardDecoder)

                "pair" ->
                    Json.Decode.map5 Pair
                        (Json.Decode.field "pair1" cardDecoder)
                        (Json.Decode.field "pair2" cardDecoder)
                        (Json.Decode.field "kicker1" cardDecoder)
                        (Json.Decode.field "kicker2" cardDecoder)
                        (Json.Decode.field "kicker3" cardDecoder)

                "two-pair" ->
                    Json.Decode.map5 TwoPair
                        (Json.Decode.field "up1" cardDecoder)
                        (Json.Decode.field "up2" cardDecoder)
                        (Json.Decode.field "down1" cardDecoder)
                        (Json.Decode.field "down2" cardDecoder)
                        (Json.Decode.field "kicker" cardDecoder)

                "three-of-a-kind" ->
                    Json.Decode.map5 ThreeOfAKind
                        (Json.Decode.field "trip1" cardDecoder)
                        (Json.Decode.field "trip2" cardDecoder)
                        (Json.Decode.field "trip3" cardDecoder)
                        (Json.Decode.field "kicker1" cardDecoder)
                        (Json.Decode.field "kicker2" cardDecoder)

                "straight" ->
                    Json.Decode.map5 Straight
                        (Json.Decode.field "high" cardDecoder)
                        (Json.Decode.field "next1" cardDecoder)
                        (Json.Decode.field "next2" cardDecoder)
                        (Json.Decode.field "next3" cardDecoder)
                        (Json.Decode.field "low" cardDecoder)

                "flush" ->
                    Json.Decode.map5 Flush
                        (Json.Decode.field "high" cardDecoder)
                        (Json.Decode.field "next1" cardDecoder)
                        (Json.Decode.field "next2" cardDecoder)
                        (Json.Decode.field "next3" cardDecoder)
                        (Json.Decode.field "low" cardDecoder)

                "full-house" ->
                    Json.Decode.map5 FullHouse
                        (Json.Decode.field "trip1" cardDecoder)
                        (Json.Decode.field "trip2" cardDecoder)
                        (Json.Decode.field "trip3" cardDecoder)
                        (Json.Decode.field "pair1" cardDecoder)
                        (Json.Decode.field "pair2" cardDecoder)

                "four-of-a-kind" ->
                    Json.Decode.map5 FourOfAKind
                        (Json.Decode.field "quad1" cardDecoder)
                        (Json.Decode.field "quad2" cardDecoder)
                        (Json.Decode.field "quad3" cardDecoder)
                        (Json.Decode.field "quad4" cardDecoder)
                        (Json.Decode.field "kicker" cardDecoder)

                "straight-flush" ->
                    Json.Decode.map5 StraightFlush
                        (Json.Decode.field "high" cardDecoder)
                        (Json.Decode.field "next1" cardDecoder)
                        (Json.Decode.field "next2" cardDecoder)
                        (Json.Decode.field "next3" cardDecoder)
                        (Json.Decode.field "low" cardDecoder)

                _ ->
                    Json.Decode.fail "Couldn't understand the server's description of your hand."
    in
    Json.Decode.field "hand" Json.Decode.string
        |> Json.Decode.andThen decode


posixDecoder : Json.Decode.Decoder Time.Posix
posixDecoder =
    Json.Decode.map Time.millisToPosix Json.Decode.int


timerLevelDecoder : Json.Decode.Decoder TimerLevel
timerLevelDecoder =
    let
        roundLevelDecoder =
            Json.Decode.map2 RoundLevel
                (Json.Decode.field "durationSeconds" Json.Decode.int)
                (Json.Decode.field "smallBlind" Json.Decode.int)

        breakLevelDecoder =
            Json.Decode.map BreakLevel
                (Json.Decode.field "durationSeconds" Json.Decode.int)
    in
    Json.Decode.oneOf
        [ roundLevelDecoder
        , breakLevelDecoder
        ]


timerStatusDecoder : Json.Decode.Decoder TimerStatus
timerStatusDecoder =
    Json.Decode.succeed TimerStatus
        |> required "timerStartTime" posixDecoder
        |> required "pausedTime" (Json.Decode.nullable posixDecoder)
        |> required "levels" (Json.Decode.list timerLevelDecoder)


rankDecoder : Json.Decode.Decoder Rank
rankDecoder =
    let
        get id =
            case id of
                "2" ->
                    Json.Decode.succeed Two

                "3" ->
                    Json.Decode.succeed Three

                "4" ->
                    Json.Decode.succeed Four

                "5" ->
                    Json.Decode.succeed Five

                "6" ->
                    Json.Decode.succeed Six

                "7" ->
                    Json.Decode.succeed Seven

                "8" ->
                    Json.Decode.succeed Eight

                "9" ->
                    Json.Decode.succeed Nine

                "10" ->
                    Json.Decode.succeed Ten

                "J" ->
                    Json.Decode.succeed Jack

                "Q" ->
                    Json.Decode.succeed Queen

                "K" ->
                    Json.Decode.succeed King

                "A" ->
                    Json.Decode.succeed Ace

                _ ->
                    Json.Decode.fail "Couldn't understand the server's description of a card's rank."
    in
    Json.Decode.string |> Json.Decode.andThen get


suitDecoder : Json.Decode.Decoder Suit
suitDecoder =
    let
        get id =
            case id of
                "♣" ->
                    Json.Decode.succeed Clubs

                "♦" ->
                    Json.Decode.succeed Diamonds

                "♥" ->
                    Json.Decode.succeed Hearts

                "♠" ->
                    Json.Decode.succeed Spades

                _ ->
                    Json.Decode.fail "Couldn't understand the server's description of a card's suit."
    in
    Json.Decode.string |> Json.Decode.andThen get


cardDecoder : Json.Decode.Decoder Card
cardDecoder =
    Json.Decode.succeed Card
        |> required "rank" rankDecoder
        |> required "suit" suitDecoder


holeDecoder : Json.Decode.Decoder ( Card, Card )
holeDecoder =
    Json.Decode.succeed Tuple.pair
        |> required "card1" cardDecoder
        |> required "card2" cardDecoder


decodeWelcome : Posix -> Json.Decode.Decoder Message
decodeWelcome now =
    Json.Decode.map3 WelcomeMessage
        (serverWelcomeDecoder now)
        (Json.Decode.field "self" selfDecoder)
        (Json.Decode.field "game" gameDecoder)


playerKeyDecoder : Json.Decode.Decoder PlayerKey
playerKeyDecoder =
    Json.Decode.map Pkey Json.Decode.string


playerIdDecoder : Json.Decode.Decoder PlayerId
playerIdDecoder =
    Json.Decode.map Pid Json.Decode.string


gameIdDecoder : Json.Decode.Decoder GameId
gameIdDecoder =
    Json.Decode.map Gid Json.Decode.string


failureDecoder : Json.Decode.Decoder Failure
failureDecoder =
    Json.Decode.succeed Failure
        |> required "message" Json.Decode.string
        |> optional "context" (Json.Decode.nullable Json.Decode.string) Nothing


playerGameStatusMessageDecoder : Json.Decode.Decoder Message
playerGameStatusMessageDecoder =
    Json.Decode.map3 PlayerGameStatusMessage
        (Json.Decode.field "self" selfDecoder)
        (Json.Decode.field "game" gameDecoder)
        (Json.Decode.field "action" actionDecoder)


spectatorGameStatusMessageDecoder : Json.Decode.Decoder Message
spectatorGameStatusMessageDecoder =
    Json.Decode.map3 SpectatorGameStatusMessage
        (Json.Decode.field "self" spectatorDecoder)
        (Json.Decode.field "game" gameDecoder)
        (Json.Decode.field "action" actionDecoder)


playerRoundWinningsMessageDecoder : Json.Decode.Decoder Message
playerRoundWinningsMessageDecoder =
    Json.Decode.map4 PlayerRoundWinningsMessage
        (Json.Decode.field "self" selfDecoder)
        (Json.Decode.field "game" gameDecoder)
        (Json.Decode.field "pots" (Json.Decode.list potResultDecoder))
        (Json.Decode.field "players" (Json.Decode.list playerWinningsDecoder))


spectatorRoundWinningsMessageDecoder : Json.Decode.Decoder Message
spectatorRoundWinningsMessageDecoder =
    Json.Decode.map4 SpectatorRoundWinningsMessage
        (Json.Decode.field "self" spectatorDecoder)
        (Json.Decode.field "game" gameDecoder)
        (Json.Decode.field "pots" (Json.Decode.list potResultDecoder))
        (Json.Decode.field "players" (Json.Decode.list playerWinningsDecoder))


statusMessageDecoder : Json.Decode.Decoder Message
statusMessageDecoder =
    Json.Decode.map StatusMessage <|
        Json.Decode.field "message" Json.Decode.string


failureMessageDecoder : Json.Decode.Decoder Message
failureMessageDecoder =
    Json.Decode.map FailureMessage <|
        Json.Decode.field "failures" (Json.Decode.list failureDecoder)


messageDecoder : Posix -> Json.Decode.Decoder Message
messageDecoder now =
    Json.Decode.oneOf
        [ decodeWelcome now
        , playerGameStatusMessageDecoder
        , spectatorGameStatusMessageDecoder
        , playerRoundWinningsMessageDecoder
        , spectatorRoundWinningsMessageDecoder
        , statusMessageDecoder
        , failureMessageDecoder
        ]



-- Request encoders


encodeGameId : GameId -> Json.Encode.Value
encodeGameId (Gid string) =
    Json.Encode.string string


encodePlayerId : PlayerId -> Json.Encode.Value
encodePlayerId (Pid string) =
    Json.Encode.string string


encodePlayerKey : PlayerKey -> Json.Encode.Value
encodePlayerKey (Pkey string) =
    Json.Encode.string string


encodeTimerLevel : TimerLevel -> Json.Encode.Value
encodeTimerLevel timerLevel =
    case timerLevel of
        RoundLevel durationSeconds smallBlind ->
            Json.Encode.object
                [ ( "durationSeconds", Json.Encode.int durationSeconds )
                , ( "smallBlind", Json.Encode.int smallBlind )
                ]

        BreakLevel durationSeconds ->
            Json.Encode.object
                [ ( "durationSeconds", Json.Encode.int durationSeconds ) ]


encodePosix : Posix -> Json.Encode.Value
encodePosix posix =
    Json.Encode.int <| Time.posixToMillis posix


encodeTimerStatus : TimerStatus -> Json.Encode.Value
encodeTimerStatus timerStatus =
    Json.Encode.object <|
        [ ( "timerStartTime", encodePosix timerStatus.timerStartTime )
        , ( "pausedTime", (Maybe.map encodePosix >> Maybe.withDefault Json.Encode.null) timerStatus.pausedTime )
        , ( "levels", Json.Encode.list encodeTimerLevel timerStatus.levels )
        ]


welcomeEncoder : Welcome -> Json.Encode.Value
welcomeEncoder welcome =
    Json.Encode.object <|
        [ ( "playerKey", encodePlayerKey welcome.playerKey )
        , ( "playerId", encodePlayerId welcome.playerId )
        , ( "gameId", encodeGameId welcome.gameId )
        , ( "gameCode", Json.Encode.string welcome.gameCode )
        , ( "gameName", Json.Encode.string welcome.gameName )
        , ( "screenName", Json.Encode.string welcome.screenName )
        , ( "spectator", Json.Encode.bool welcome.spectator )
        , ( "joined", Json.Encode.int <| posixToMillis welcome.joined )
        ]


createGameRequestEncoder : CreateGameRequest -> Json.Encode.Value
createGameRequestEncoder createGameRequest =
    Json.Encode.object <|
        [ ( "operation", Json.Encode.string "create-game" )
        , ( "screenName", Json.Encode.string createGameRequest.screenName )
        , ( "gameName", Json.Encode.string createGameRequest.gameName )
        ]


joinGameRequestEncoder : JoinGameRequest -> Json.Encode.Value
joinGameRequestEncoder joinGameRequest =
    Json.Encode.object <|
        [ ( "operation", Json.Encode.string "join-game" )
        , ( "gameCode", Json.Encode.string joinGameRequest.gameCode )
        , ( "screenName", Json.Encode.string joinGameRequest.screenName )
        ]


startGameRequestEncoder : StartGameRequest -> Json.Encode.Value
startGameRequestEncoder startGameRequest =
    Json.Encode.object <|
        [ ( "operation", Json.Encode.string "start-game" )
        , ( "gameId", encodeGameId startGameRequest.gameId )
        , ( "playerId", encodePlayerId startGameRequest.playerId )
        , ( "playerKey", encodePlayerKey startGameRequest.playerKey )
        , ( "startingStack", (Maybe.map Json.Encode.int >> Maybe.withDefault Json.Encode.null) startGameRequest.startingStack )
        , ( "initialSmallBlind", (Maybe.map Json.Encode.int >> Maybe.withDefault Json.Encode.null) startGameRequest.initialSmallBlind )
        , ( "timerConfig", (Maybe.map (Json.Encode.list encodeTimerLevel) >> Maybe.withDefault Json.Encode.null) startGameRequest.timerConfig )
        , ( "playerOrder", Json.Encode.list encodePlayerId startGameRequest.playerOrder )
        ]


updateBlindRequestEncoder : UpdateBlindRequest -> Json.Encode.Value
updateBlindRequestEncoder updateBlindRequest =
    Json.Encode.object <|
        [ ( "operation", Json.Encode.string "update-blind" )
        , ( "gameId", encodeGameId updateBlindRequest.gameId )
        , ( "playerId", encodePlayerId updateBlindRequest.playerId )
        , ( "playerKey", encodePlayerKey updateBlindRequest.playerKey )
        , ( "timerLevels", (Maybe.map (Json.Encode.list encodeTimerLevel) >> Maybe.withDefault Json.Encode.null) updateBlindRequest.timerLevels )
        , ( "progress", (Maybe.map Json.Encode.int >> Maybe.withDefault Json.Encode.null) updateBlindRequest.progress )
        , ( "smallBlind", (Maybe.map Json.Encode.int >> Maybe.withDefault Json.Encode.null) updateBlindRequest.smallBlind )
        , ( "playing", (Maybe.map Json.Encode.bool >> Maybe.withDefault Json.Encode.null) updateBlindRequest.playing )
        ]


betRequestEncoder : BetRequest -> Json.Encode.Value
betRequestEncoder betRequest =
    Json.Encode.object <|
        [ ( "operation", Json.Encode.string "bet" )
        , ( "gameId", encodeGameId betRequest.gameId )
        , ( "playerKey", encodePlayerKey betRequest.playerKey )
        , ( "playerId", encodePlayerId betRequest.playerId )
        , ( "betAmount", Json.Encode.int betRequest.betAmount )
        ]


checkRequestEncoder : CheckRequest -> Json.Encode.Value
checkRequestEncoder checkRequest =
    Json.Encode.object <|
        [ ( "operation", Json.Encode.string "check" )
        , ( "gameId", encodeGameId checkRequest.gameId )
        , ( "playerKey", encodePlayerKey checkRequest.playerKey )
        , ( "playerId", encodePlayerId checkRequest.playerId )
        ]


foldRequestEncoder : FoldRequest -> Json.Encode.Value
foldRequestEncoder foldRequest =
    Json.Encode.object <|
        [ ( "operation", Json.Encode.string "fold" )
        , ( "gameId", encodeGameId foldRequest.gameId )
        , ( "playerKey", encodePlayerKey foldRequest.playerKey )
        , ( "playerId", encodePlayerId foldRequest.playerId )
        ]


advancePhaseRequestEncoder : AdvancePhaseRequest -> Json.Encode.Value
advancePhaseRequestEncoder advancePhaseRequest =
    Json.Encode.object <|
        [ ( "operation", Json.Encode.string "advance-phase" )
        , ( "gameId", encodeGameId advancePhaseRequest.gameId )
        , ( "playerKey", encodePlayerKey advancePhaseRequest.playerKey )
        , ( "playerId", encodePlayerId advancePhaseRequest.playerId )
        ]


pingRequestEncoder : PingRequest -> Json.Encode.Value
pingRequestEncoder pingRequest =
    Json.Encode.object <|
        [ ( "operation", Json.Encode.string "ping" )
        , ( "gameId", encodeGameId pingRequest.gameId )
        , ( "playerId", encodePlayerId pingRequest.playerId )
        , ( "playerKey", encodePlayerKey pingRequest.playerKey )
        ]


wakeRequestEncoder : () -> Json.Encode.Value
wakeRequestEncoder _ =
    Json.Encode.object <|
        [ ( "operation", Json.Encode.string "wake" )
        ]
