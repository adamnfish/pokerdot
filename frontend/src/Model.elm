module Model exposing (..)

import Browser.Dom exposing (Viewport)
import Json.Decode
import Json.Encode
import Time


type Msg
    = NoOp
    | Tick Time.Posix
    | OnResize
    | Resized Viewport
      -- connections
    | ServerMessage Json.Encode.Value
    | SocketConnect
    | SocketDisconnect
      -- basic navigation
    | NavigateHome
    | NavigateHelp
    | NavigateGame -- ???
      -- create game
    | NavigateCreateGame
    | InputCreateGame String String
    | SubmitCreateGame String String
      -- join game
    | NavigateJoinGame
    | InputJoinGame String String
    | SubmitJoinGame String String
      -- lobby
    | InputReorderPlayers (List Player)
    | SubmitStartGame
      -- game messages
    | TogglePeek
    | Check
    | Bet Int
    | Fold


type alias Model =
    { ui : UI
    , connected : Bool
    , now : Time.Posix
    , viewport : Viewport
    , peeking : Bool
    , loadingStatus : LoadingStatus
    , library : List Welcome
    }


type UI
    = WelcomeScreen
    | HelpScreen
    | CreateGameScreen
    | LobbyScreen (List Player) (Maybe ( Self, Game )) Welcome
    | RejoinScreen Welcome
    | WaitingGameScreen PlayerId Self Game Welcome
    | ActingGameScreen ActSelection Self Game Welcome
      -- spectating
    | CommunityCardsScreen Game Welcome
    | TimerScreen TimerStatus Game Welcome
    | ChipSummaryScreen Game Welcome


type LoadingStatus
    = NotLoading
    | AwaitingMessage


type alias Error =
    { message : String
    , time : Time.Posix
    }


type GameId
    = Gid String


type PlayerId
    = Pid String


type PlayerKey
    = Pkey String


type alias Player =
    { playerId : PlayerId
    , screenName : String
    , stack : Int
    , pot : Int
    , bid : Int
    , folded : Bool
    , busted : Bool
    }


type alias Spectator =
    { playerId : PlayerId
    }


type alias Welcome =
    { playerKey : PlayerKey
    , playerId : PlayerId
    , gameId : GameId
    , gameName : String
    , screenName : String
    , spectator : Bool
    }


type alias Self =
    { playerId : PlayerId
    , screenName : String
    , stack : Int
    , pot : Int
    , bid : Int
    , folded : Bool
    , busted : Bool
    , hole : ( Card, Card )
    }


type alias Game =
    { gameId : GameId
    , gameName : String
    , players : List Player
    , spectators : List Spectator
    , round : Round
    , inTurn : Maybe Player
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
    = RoundPhase Int Int
    | Break Int


type Round
    = PreFlopSummary
    | FlopSummary Card Card Card
    | TurnSummary Card Card Card Card
    | RiverSummary Card Card Card Card Card
    | ShowdownSummary Card Card Card Card Card (List ( PlayerId, Card, Card ))



-- UI states


type ActSelection
    = ActBet Int
    | ActCall
    | ActFold



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
