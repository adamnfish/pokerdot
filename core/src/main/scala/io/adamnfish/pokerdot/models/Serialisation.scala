package io.adamnfish.pokerdot.models

import cats.Functor.ops.toAllFunctorOps
import io.circe.Json.JString
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder, Json, JsonObject, KeyEncoder, parser}
import io.circe.syntax._
import zio.IO


object Serialisation {
  def encodeMessage(message: Message): String = {
    message.asJson.noSpaces
  }

  def encodeFailure(failures: Failures): String = {
    failures.asJson.noSpaces
  }

  def parse(jsonStr: String, userMessage: String, context: Option[String]): Either[Failures, Json] = {
    parser.parse(jsonStr).left.map { parsingFailure =>
      Failures(
        s"Failed to parse request body JSON: ${parsingFailure.message}",
        userMessage,
        context,
        Some(parsingFailure)
      )
    }
  }

  def extractJson[A](json: Json, userMessage: String)(implicit decoder: Decoder[A]): Either[Failures, A] = {
    json.as[A].left.map { decodingFailure =>
      Failures(
        s"Failed to parse JSON as expected type: ${decodingFailure.message}",
        userMessage,
        Some(decodingFailure.history.mkString("|")),
        Some(decodingFailure)
      )
    }
  }

  // REQUEST PARSERS

  def parseCreateGameRequest(json: Json): Either[Failures, CreateGame] = {
    extractJson[CreateGame](json, "Could not understand the create game request")
  }

  def parseJoinGameRequest(json: Json): Either[Failures, JoinGame] = {
    extractJson[JoinGame](json, "Could not understand the join game request")
  }

  def parseStartGameRequest(json: Json): Either[Failures, StartGame] = {
    extractJson[StartGame](json, "Could not understand the start game request")
  }

  def parseUpdateTimeRequest(json: Json): Either[Failures, UpdateTimer] = {
    extractJson[UpdateTimer](json, "Could not understand the update time request")
  }

  def parseBidRequest(json: Json): Either[Failures, Bid] = {
    extractJson[Bid](json, "Could not understand the bid request")
  }

  def parseCheckRequest(json: Json): Either[Failures, Check] = {
    extractJson[Check](json, "Could not understand the check request")
  }

  def parseFoldRequest(json: Json): Either[Failures, Fold] = {
    extractJson[Fold](json, "Could not understand the fold request")
  }

  def parseAdvancePhaseRequest(json: Json): Either[Failures, AdvancePhase] = {
    extractJson[AdvancePhase](json, "Could not understand the advance phase request")
  }

  def parsePingRequest(json: Json): Either[Failures, Ping] = {
    extractJson[Ping](json, "Could not understand the ping request")
  }



  // VALUE CLASSES
  private implicit val playerIdEncoder: Encoder[PlayerId] = Encoder.encodeString.contramap[PlayerId](_.pid)
  private implicit val playerKeyEncoder: Encoder[PlayerKey] = Encoder.encodeString.contramap[PlayerKey](_.key)
  private implicit val playerAddressEncoder: Encoder[PlayerAddress] = Encoder.encodeString.contramap[PlayerAddress](_.address)
  private implicit val gameIdEncoder: Encoder[GameId] = Encoder.encodeString.contramap[GameId](_.gid)

  private implicit val playerIdDecoder: Decoder[PlayerId] = Decoder.decodeString.emap[PlayerId](str => Right(PlayerId(str)))
  private implicit val playerKeyDecoder: Decoder[PlayerKey] = Decoder.decodeString.emap[PlayerKey](str => Right(PlayerKey(str)))
  private implicit val playerAddressDecoder: Decoder[PlayerAddress] = Decoder.decodeString.emap[PlayerAddress](str => Right(PlayerAddress(str)))
  private implicit val gameIdDecoder: Decoder[GameId] = Decoder.decodeString.emap[GameId](str => Right(GameId(str)))

  private implicit val playerIdKeyEncoder: KeyEncoder[PlayerId] = new KeyEncoder[PlayerId] {
    override def apply(playerId: PlayerId): String = playerId.pid
  }

  // GAME TYPES
  private implicit val decodeRank: Decoder[Rank] = Decoder[String].emap {
    case "2" =>
      Right(Two)
    case "3" =>
      Right(Three)
    case "4" =>
      Right(Four)
    case "5" =>
      Right(Five)
    case "6" =>
      Right(Six)
    case "7" =>
      Right(Seven)
    case "8" =>
      Right(Eight)
    case "9" =>
      Right(Nine)
    case "10" =>
      Right(Ten)
    case "J" =>
      Right(Jack)
    case "Q" =>
      Right(Queen)
    case "K" =>
      Right(King)
    case "A" =>
      Right(Ace)
    case otherRank =>
      Left(s"Invalid rank: $otherRank")
  }
  private implicit val encodeRank: Encoder[Rank] = Encoder[String].contramap {
    case Two =>
      "2"
    case Three =>
      "3"
    case Four =>
      "4"
    case Five =>
      "5"
    case Six =>
      "6"
    case Seven =>
      "7"
    case Eight =>
      "8"
    case Nine =>
      "9"
    case Ten =>
      "10"
    case Jack =>
      "J"
    case Queen =>
      "Q"
    case King =>
      "K"
    case Ace =>
      "A"
  }
  private implicit val decodeSuit: Decoder[Suit] = Decoder[String].emap {
    case "♣" =>
      Right(Clubs)
    case "♦" =>
      Right(Diamonds)
    case "♥" =>
      Right(Hearts)
    case "♠" =>
      Right(Spades)
    case otherSuit =>
      Left(s"Invalid suit: $otherSuit")
  }
  private implicit val encodeSuit: Encoder[Suit] = Encoder[String].contramap {
    case Clubs =>
      "♣"
    case Diamonds =>
      "♦"
    case Hearts =>
      "♥"
    case Spades =>
      "♠"
  }
  private implicit val cardEncoder: Encoder[Card] = deriveEncoder[Card]
  private implicit val cardDecoder: Decoder[Card] = deriveDecoder[Card]
  private implicit val holeEncoder: Encoder[Hole] = deriveEncoder[Hole]
  private implicit val holeDecoder: Decoder[Hole] = deriveDecoder[Hole]

  private implicit val highCardEncoder: Encoder[HighCard] = deriveEncoder[HighCard]
  private implicit val pairEncoder: Encoder[Pair] = deriveEncoder[Pair]
  private implicit val twoPairEncoder: Encoder[TwoPair] = deriveEncoder[TwoPair]
  private implicit val threeOfAKindEncoder: Encoder[ThreeOfAKind] = deriveEncoder[ThreeOfAKind]
  private implicit val straightEncoder: Encoder[Straight] = deriveEncoder[Straight]
  private implicit val flushEncoder: Encoder[Flush] = deriveEncoder[Flush]
  private implicit val fullHouseEncoder: Encoder[FullHouse] = deriveEncoder[FullHouse]
  private implicit val fourOfAKindEncoder: Encoder[FourOfAKind] = deriveEncoder[FourOfAKind]
  private implicit val straightFlushEncoder: Encoder[StraightFlush] = deriveEncoder[StraightFlush]
  private implicit val handEncoder: Encoder[Hand] = Encoder.instance {
    case highCard: HighCard =>
      highCardEncoder.apply(highCard)
        .mapObject(o => o.add("hand", Json.fromString("high-card")))
    case pair: Pair =>
      pairEncoder.apply(pair)
        .mapObject(o => o.add("hand", Json.fromString("pair")))
    case twoPair: TwoPair =>
      twoPairEncoder.apply(twoPair)
        .mapObject(o => o.add("hand", Json.fromString("two-pair")))
    case threeOfAKind: ThreeOfAKind =>
      threeOfAKindEncoder.apply(threeOfAKind)
        .mapObject(o => o.add("hand", Json.fromString("three-of-a-kind")))
    case straight: Straight =>
      straightEncoder.apply(straight)
        .mapObject(o => o.add("hand", Json.fromString("straight")))
    case flush: Flush =>
      flushEncoder.apply(flush)
        .mapObject(o => o.add("hand", Json.fromString("flush")))
    case fullHouse: FullHouse =>
      fullHouseEncoder.apply(fullHouse)
        .mapObject(o => o.add("hand", Json.fromString("full-house")))
    case fourOfAKind: FourOfAKind =>
      fourOfAKindEncoder.apply(fourOfAKind)
        .mapObject(o => o.add("hand", Json.fromString("four-of-a-kind")))
    case straightFlush: StraightFlush =>
      straightFlushEncoder.apply(straightFlush)
        .mapObject(o => o.add("hand", Json.fromString("straight-flush")))
  }

  private implicit val TimerStatusEncoder: Encoder[TimerStatus] = deriveEncoder[TimerStatus]
  private implicit val TimerStatusDecoder: Decoder[TimerStatus] = deriveDecoder[TimerStatus]
  private implicit val roundPhaseEncoder: Encoder[RoundLevel] = deriveEncoder[RoundLevel]
  private implicit val roundPhaseDecoder: Decoder[RoundLevel] = deriveDecoder[RoundLevel]
  private implicit val breakEncoder: Encoder[BreakLevel] = deriveEncoder[BreakLevel]
  private implicit val breakDecoder: Decoder[BreakLevel] = deriveDecoder[BreakLevel]
  private implicit val timerLevelEncoder: Encoder[TimerLevel] = Encoder.instance {
    case roundPhase: RoundLevel =>
      roundPhaseEncoder.apply(roundPhase)
    case break: BreakLevel =>
      breakEncoder.apply(break)
  }
  private implicit val timerLevelDecoder: Decoder[TimerLevel] = {
    Decoder[RoundLevel].widen or Decoder[BreakLevel].widen
  }


  private implicit val playerWinningsEncoder: Encoder[PlayerWinnings] = deriveEncoder
  private implicit val potWinningsEncoder: Encoder[PotWinnings] = deriveEncoder

  // SUMMARY TYPES
  private implicit val preflopSummaryEncoder: Encoder[PreFlopSummary] = deriveEncoder[PreFlopSummary]
  private implicit val flopSummaryEncoder: Encoder[FlopSummary] = deriveEncoder[FlopSummary]
  private implicit val turnSummaryEncoder: Encoder[TurnSummary] = deriveEncoder[TurnSummary]
  private implicit val riverSummaryEncoder: Encoder[RiverSummary] = deriveEncoder[RiverSummary]
  private implicit val showdownSummaryEncoder: Encoder[ShowdownSummary] = deriveEncoder[ShowdownSummary]
  private implicit val roundSummaryEncoder: Encoder[RoundSummary] = Encoder.instance {
    case preFlopSummary: PreFlopSummary =>
      preflopSummaryEncoder.apply(preFlopSummary)
    case flopSummary: FlopSummary =>
      flopSummaryEncoder.apply(flopSummary)
    case turnSummary: TurnSummary =>
      turnSummaryEncoder.apply(turnSummary)
    case riverSummary: RiverSummary =>
      riverSummaryEncoder.apply(riverSummary)
    case showdownSummary: ShowdownSummary =>
      showdownSummaryEncoder.apply(showdownSummary)
  }

  private implicit val playerJoinedSummaryEncoder: Encoder[PlayerJoinedSummary] = deriveEncoder[PlayerJoinedSummary]
  private implicit val betSummaryEncoder: Encoder[BetSummary] = deriveEncoder[BetSummary]
  private implicit val checkSummaryEncoder: Encoder[CheckSummary] = deriveEncoder[CheckSummary]
  private implicit val foldSummaryEncoder: Encoder[FoldSummary] = deriveEncoder[FoldSummary]
  private implicit val advancePhaseSummaryEncoder: Encoder[AdvancePhaseSummary] = deriveEncoder[AdvancePhaseSummary]
  private implicit val noActionSummaryEncoder: Encoder[NoActionSummary] = deriveEncoder[NoActionSummary]
  private implicit val actionSummaryEncoder: Encoder[ActionSummary] = Encoder.instance {
    case playerJoinedSummary: PlayerJoinedSummary =>
      playerJoinedSummaryEncoder.apply(playerJoinedSummary)
        .mapObject(o => o.add("action", Json.fromString("player-joined")))
    case betSummary: BetSummary =>
      betSummaryEncoder.apply(betSummary)
        .mapObject(o => o.add("action", Json.fromString("bet")))
    case checkSummary: CheckSummary =>
      checkSummaryEncoder.apply(checkSummary)
        .mapObject(o => o.add("action", Json.fromString("check")))
    case foldSummary: FoldSummary =>
      foldSummaryEncoder.apply(foldSummary)
        .mapObject(o => o.add("action", Json.fromString("fold")))
    case advancePhaseSummary: AdvancePhaseSummary =>
      advancePhaseSummaryEncoder.apply(advancePhaseSummary)
        .mapObject(o => o.add("action", Json.fromString("advance-phase")))
    case noActionSummary: NoActionSummary =>
      noActionSummaryEncoder.apply(noActionSummary)
        .mapObject(o => o.add("action", Json.fromString("no-action")))
  }

  private implicit val playerSummaryEncoder: Encoder[PlayerSummary] = deriveEncoder[PlayerSummary]
  private implicit val spectatorSummaryEncoder: Encoder[SpectatorSummary] = deriveEncoder[SpectatorSummary]
  private implicit val selfSummaryEncoder: Encoder[SelfSummary] = deriveEncoder[SelfSummary]
  private implicit val gameSummaryEncoder: Encoder[GameSummary] = deriveEncoder[GameSummary]

  private implicit val selfEncoder: Encoder[Self] = Encoder.instance {
    case self: SelfSummary =>
      selfSummaryEncoder.apply(self)
    case self: SpectatorSummary =>
      spectatorSummaryEncoder.apply(self)
  }

  // MESSAGES
  private implicit val welcomeEncoder: Encoder[Welcome] = deriveEncoder
  private implicit val gameStatusEncoder: Encoder[GameStatus] = deriveEncoder
  private implicit val roundWinningsEncoder: Encoder[RoundWinnings] = deriveEncoder
  private implicit val statusEncoder: Encoder[Status] = deriveEncoder

  private implicit val messageEncoder: Encoder[Message] = Encoder.instance {
    case welcome: Welcome =>
      welcomeEncoder.apply(welcome)
    case gameStatus: GameStatus =>
      gameStatusEncoder.apply(gameStatus)
    case roundWinnings: RoundWinnings =>
      roundWinningsEncoder.apply(roundWinnings)
    case status: Status =>
      statusEncoder.apply(status)
  }

  // REQUESTS
  private implicit val createGameDecoder: Decoder[CreateGame] = deriveDecoder[CreateGame]
  private implicit val joinGameDecoder: Decoder[JoinGame] = deriveDecoder[JoinGame]
  private implicit val startGameDecoder: Decoder[StartGame] = deriveDecoder[StartGame]
  private implicit val updateTimeDecoder: Decoder[UpdateTimer] = deriveDecoder[UpdateTimer]
  private implicit val bidDecoder: Decoder[Bid] = deriveDecoder[Bid]
  private implicit val checkDecoder: Decoder[Check] = deriveDecoder[Check]
  private implicit val foldDecoder: Decoder[Fold] = deriveDecoder[Fold]
  private implicit val advancePhaseDecoder: Decoder[AdvancePhase] = deriveDecoder[AdvancePhase]
  private implicit val pingDecoder: Decoder[Ping] = deriveDecoder[Ping]

  // FAILURE
  private implicit val failureEncoder: Encoder[Failure] = Encoder.encodeJsonObject.contramap { failure =>
    val message =
      Json.fromString(failure.userMessage)
    failure.context match {
      case Some(context) =>
        JsonObject(
          "message" -> message,
          "context" -> Json.fromString(context)
        )
      case None =>
        JsonObject(
          "message" -> message
        )
    }
  }
  private implicit val failuresEncoder: Encoder[Failures] = deriveEncoder

  object RequestEncoders {
    implicit val createGameEncoder: Encoder[CreateGame] = deriveEncoder[CreateGame]
    implicit val joinGameEncoder: Encoder[JoinGame] = deriveEncoder[JoinGame]
    implicit val startGameEncoder: Encoder[StartGame] = deriveEncoder[StartGame]
    implicit val updateTimeEncoder: Encoder[UpdateTimer] = deriveEncoder[UpdateTimer]
    implicit val bidEncoder: Encoder[Bid] = deriveEncoder[Bid]
    implicit val checkEncoder: Encoder[Check] = deriveEncoder[Check]
    implicit val foldEncoder: Encoder[Fold] = deriveEncoder[Fold]
    implicit val advancePhaseEncoder: Encoder[AdvancePhase] = deriveEncoder[AdvancePhase]
    implicit val pingEncoder: Encoder[Ping] = deriveEncoder[Ping]
  }
}
