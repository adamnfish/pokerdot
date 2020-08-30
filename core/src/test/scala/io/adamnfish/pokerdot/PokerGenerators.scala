package io.adamnfish.pokerdot

import io.adamnfish.pokerdot.logic.Cards.RichRank
import io.adamnfish.pokerdot.logic.PokerHands.{allRanks, allSuits, cardOrd, rankOrd}
import io.adamnfish.pokerdot.models._
import org.scalacheck.{Arbitrary, Gen}


trait PokerGenerators {
  implicit val rankGen: Gen[Rank] =
    Gen.oneOf(allRanks)

  implicit val arbRank: Arbitrary[Rank] =
    Arbitrary(rankGen)

  implicit val suitGen: Gen[Suit] =
    Gen.oneOf(allSuits)

  implicit val arbSuit: Arbitrary[Suit] =
    Arbitrary(suitGen)

  implicit val cardGen: Gen[Card] =
    for {
      rank <- rankGen
      suit <- suitGen
    } yield rank of suit

  implicit val arbCard: Arbitrary[Card] =
    Arbitrary(cardGen)

  /**
   * Generates 7 cards that do not connect.
   * All distinct ranks, no flush, and no straights.
   */
  val nothingConnectsCardsGen: Gen[List[Card]] = {
    val gen = for {
      // all ranks must be distinct
      rank1 <- rankGen
      rank2 <- Gen.oneOf(
        allRanks.filterNot(_ == rank1)
      )
      rank3 <- Gen.oneOf(
        allRanks.filterNot(Set(rank1, rank2).contains)
      )
      rank4 <- Gen.oneOf(
        allRanks.filterNot(Set(rank1, rank2, rank3).contains)
      )
      rank5 <- Gen.oneOf(
        allRanks.filterNot(
          Set(rank1, rank2, rank3, rank4)
            .union(breakStraight(rank1, rank2, rank3, rank4))
            .contains
        )
      )
      rank6 <- Gen.oneOf(
        allRanks.filterNot(
          Set(rank1, rank2, rank3, rank4, rank5)
            .union(breakStraight(rank1, rank2, rank3, rank4, rank5))
            .contains
        )
      )
      rank7 <- Gen.oneOf(
        allRanks.filterNot(
          Set(rank1, rank2, rank3, rank4, rank5, rank6)
            .union(breakStraight(rank1, rank2, rank3, rank4, rank5, rank6))
            .contains
        )
      )

      suit1 <- suitGen
      suit2 <- suitGen
      suit3 <- suitGen
      suit4 <- suitGen
      suit5 <- Gen.oneOf(
        allSuits.filterNot(breakFlush(suit1, suit2, suit3, suit4).contains)
      )
      suit6 <- Gen.oneOf(
        allSuits.filterNot(breakFlush(suit1, suit2, suit3, suit4, suit5).contains)
      )
      suit7 <- Gen.oneOf(
        allSuits.filterNot(breakFlush(suit1, suit2, suit3, suit4, suit5, suit6).contains)
      )
    } yield orderCards(
      List(
        rank1 of suit1, rank2 of suit2, rank3 of suit3, rank4 of suit4,
        rank5 of suit5, rank6 of suit6, rank7 of suit7,
      )
    )
    gen.suchThat(cards => cards.size == 7 && cards.distinct.size == 7)
  }

  /**
   * Generates 7 cards that contains a single pair.
   * All distinct ranks apart from the pair, no flush, and no straights.
   */
  val pairCardsGen: Gen[List[Card]] = {
    val gen = for {
      pairRank <- rankGen
      pairSuit1 <- suitGen
      pairSuit2 <- Gen.oneOf(
        allSuits.filterNot(_ == pairSuit1)
      )
      kicker1Rank <- Gen.oneOf(
        allRanks.filterNot(_ == pairRank)
      )
      kicker1Suit <- suitGen
      kicker2Rank <- Gen.oneOf(
        allRanks.filterNot(Set(pairRank, kicker1Rank).contains)
      )
      kicker2Suit <- suitGen
      kicker3Rank <- Gen.oneOf(
        allRanks.filterNot(Set(pairRank, kicker1Rank, kicker2Rank).contains)
      )
      kicker3Suit <- Gen.oneOf(
        allSuits.filterNot(breakFlush(pairSuit1, pairSuit2, kicker1Suit, kicker2Suit))
      )
      kicker4Rank <- Gen.oneOf(
        allRanks.filterNot(
          Set(pairRank, kicker1Rank, kicker2Rank, kicker3Rank)
            .union(breakStraight(pairRank, kicker1Rank, kicker2Rank, kicker3Rank))
            .contains
        )
      )
      kicker4Suit <- Gen.oneOf(
        allSuits.filterNot(breakFlush(pairSuit1, pairSuit2, kicker1Suit, kicker2Suit, kicker3Suit))
      )
      kicker5Rank <- Gen.oneOf(
        allRanks.filterNot(
          Set(pairRank, kicker1Rank, kicker2Rank, kicker3Rank, kicker4Rank)
            .union(breakStraight(pairRank, kicker1Rank, kicker2Rank, kicker3Rank, kicker4Rank))
            .contains
        )
      )
      kicker5Suit <- Gen.oneOf(
        allSuits.filterNot(breakFlush(pairSuit1, pairSuit2, kicker1Suit, kicker2Suit, kicker3Suit, kicker4Suit))
      )
    } yield {
      orderCards(List(
        pairRank of pairSuit1, pairRank of pairSuit2,
        kicker1Rank of kicker1Suit, kicker2Rank of kicker2Suit, kicker3Rank of kicker3Suit,
        kicker4Rank of kicker4Suit, kicker5Rank of kicker5Suit
      ))
    }
    gen.suchThat(cards => cards.size == 7 && cards.distinct.size == 7)
  }

  /**
   * Generates 7 cards that contains two (or more) pairs.
   * No three (or more) of a kind, no flush, and no straights.
   */
  val twoPairCardsGen: Gen[List[Card]] = {
    val gen = for {
      pair1Rank <- rankGen
      pair1Suit1 <- suitGen
      pair1Suit2 <- Gen.oneOf(
        allSuits.filterNot(_ == pair1Suit1)
      )

      pair2Rank <- Gen.oneOf(
        allRanks.filterNot(_ == pair1Rank)
      )
      pair2Suit1 <- suitGen
      pair2Suit2 <- Gen.oneOf(
        allSuits.filterNot(_ == pair2Suit1)
      )

      kicker1Rank <- Gen.oneOf(
        allRanks.filterNot(Set(pair1Rank, pair2Rank).contains)
      )
      kicker1Suit <- Gen.oneOf(
        allSuits.filterNot(breakFlush(pair1Suit1, pair1Suit2, pair2Suit1, pair2Suit2))
      )
      kicker2Rank <- Gen.oneOf(
        allRanks.filterNot(Set(pair1Rank, pair2Rank, kicker1Rank).contains)
      )
      kicker2Suit <- Gen.oneOf(
        allSuits.filterNot(
          breakFlush(pair1Suit1, pair1Suit2, pair2Suit1, pair2Suit2, kicker1Suit)
        )
      )
      kicker3Rank <- Gen.oneOf(
        allRanks.filterNot(
          Set(pair1Rank, pair2Rank, kicker1Rank, kicker2Rank)
            .union(breakStraight(pair1Rank, pair2Rank, kicker1Rank, kicker2Rank))
            .contains
        )
      )
      kicker3Suit <- Gen.oneOf(
        allSuits.filterNot(breakFlush(pair1Suit1, pair1Suit2, pair2Suit1, pair2Suit2, kicker1Suit, kicker2Suit))
      )
    } yield {
      orderCards(List(
        pair1Rank of pair1Suit1, pair1Rank of pair1Suit2,
        pair2Rank of pair2Suit1, pair2Rank of pair2Suit2,
        kicker1Rank of kicker1Suit,
        kicker2Rank of kicker2Suit, kicker3Rank of kicker3Suit
      ))
    }
    gen.suchThat(cards => cards.size == 7 && cards.distinct.size == 7)
  }

  /**
   * Generates 7 cards that contains a three of a kind.
   * No other duplicates (no pairs), no four of a kind, no flush, and no straights.
   */
  val threeOfAKindCardsGen: Gen[List[Card]] = {
    val gen = for {
      tripRank <- rankGen
      tripSuit1 <- suitGen
      tripSuit2 <- Gen.oneOf(
        allSuits.filterNot(Set(tripSuit1).contains)
      )
      tripSuit3 <- Gen.oneOf(
        allSuits.filterNot(Set(tripSuit1, tripSuit2).contains)
      )
      kicker1Rank <- Gen.oneOf(
        allRanks.filterNot(_ == tripRank)
      )
      kicker1Suit <- suitGen
      kicker2Rank <- Gen.oneOf(
        allRanks.filterNot(Set(tripRank, kicker1Rank).contains)
      )
      kicker2Suit <- suitGen
      kicker3Rank <- Gen.oneOf(
        allRanks.filterNot(Set(tripRank, kicker1Rank, kicker2Rank).contains)
      )
      kicker3Suit <- suitGen
      kicker4Rank <- Gen.oneOf(
        allRanks.filterNot(
          Set(tripRank, kicker1Rank, kicker2Rank, kicker3Rank)
            .union(breakStraight(tripRank, kicker1Rank, kicker2Rank, kicker3Rank))
            .contains
        )
      )
      kicker4Suit <- Gen.oneOf(
        allSuits.filterNot(breakFlush(tripSuit1, tripSuit2, tripSuit3, kicker1Suit, kicker2Suit, kicker3Suit))
      )
    } yield {
      orderCards(List(
        tripRank of tripSuit1, tripRank of tripSuit2, tripRank of tripSuit3,
        kicker1Rank of kicker1Suit, kicker2Rank of kicker2Suit,
        kicker3Rank of kicker3Suit, kicker4Rank of kicker4Suit,
      ))
    }
    gen.suchThat(cards => cards.size == 7 && cards.distinct.size == 7)
  }

  val straightCardsGen: Gen[List[Card]] = {
    val gen = for {
      (rank1, rank2, rank3, rank4, rank5) <- Gen.oneOf(
        (Ace, King, Queen, Jack, Ten),
        (King, Queen, Jack, Ten, Nine),
        (Queen, Jack, Ten, Nine, Eight),
        (Jack, Ten, Nine, Eight, Seven),
        (Ten, Nine, Eight, Seven, Six),
        (Nine, Eight, Seven, Six, Five),
        (Eight, Seven, Six, Five, Four),
        (Seven, Six, Five, Four, Three),
        (Six, Five, Four, Three, Two),
        (Five, Four, Three, Two, Ace),
      )
      suit1 <- suitGen
      suit2 <- suitGen
      suit3 <- suitGen
      suit4 <- suitGen
      suit5 <- Gen.oneOf(
        allSuits.filterNot(breakFlush(suit1, suit2, suit3, suit4))
      )

      kicker1Rank <- Gen.oneOf(
        allRanks.filterNot(Set(rank1, rank2, rank3, rank4, rank5).contains)
      )
      kicker1Suit <- Gen.oneOf(
        allSuits.filterNot(breakFlush(suit1, suit2, suit3, suit4, suit5))
      )
      kicker2Rank <- Gen.oneOf(
        allRanks.filterNot(Set(rank1, rank2, rank3, rank4, rank5, kicker1Rank).contains)
      )
      kicker2Suit <- Gen.oneOf(
        allSuits.filterNot(breakFlush(suit1, suit2, suit3, suit4, suit5, kicker1Suit))
      )
    } yield {
      orderCards(List(
        rank1 of suit1, rank2 of suit2, rank3 of suit3, rank4 of suit4, rank5 of suit5,
        kicker1Rank of kicker1Suit, kicker2Rank of kicker2Suit
      ))
    }
    gen.suchThat(cards => cards.size == 7 && cards.distinct.size == 7)
  }

  val flushCardsGen: Gen[List[Card]] = {
    val gen = for {
      flushSuit <- suitGen
      rank1 <- rankGen
      rank2 <- Gen.oneOf(
        allRanks.filterNot(Set(rank1).contains)
      )
      rank3 <- Gen.oneOf(
        allRanks.filterNot(Set(rank1, rank2).contains)
      )
      rank4 <- Gen.oneOf(
        allRanks.filterNot(Set(rank1, rank2, rank3).contains)
      )
      rank5 <- Gen.oneOf(
        allRanks.filterNot(
          Set(rank1, rank2, rank3, rank4)
            .union(breakStraight(rank1, rank2, rank3, rank4))
            .contains
        )
      )
      kicker1Rank <- Gen.oneOf(
        allRanks.filterNot(
          breakStraight(rank1, rank2, rank3, rank4, rank5).contains
        )
      )
      kicker1Suit <- suitGen
      kicker2Rank <- Gen.oneOf(
        allRanks.filterNot(
          breakStraight(rank1, rank2, rank3, rank4, rank5, kicker1Rank).contains
        )
      )
      kicker2Suit <- suitGen
    } yield {
      orderCards(List(
        rank1 of flushSuit, rank2 of flushSuit, rank3 of flushSuit, rank4 of flushSuit, rank5 of flushSuit,
        kicker1Rank of kicker1Suit, kicker2Rank of kicker2Suit
      ))
    }
    gen.suchThat(cards => cards.size == 7 && cards.distinct.size == 7)
  }

  val fullHouseCardsGen: Gen[List[Card]] = {
    val gen = for {
      tripRank <- rankGen
      tripSuit1 <- suitGen
      tripSuit2 <- Gen.oneOf(
        allSuits.filterNot(Set(tripSuit1).contains)
      )
      tripSuit3 <- Gen.oneOf(
        allSuits.filterNot(Set(tripSuit1, tripSuit2).contains)
      )
      pairRank <- Gen.oneOf(
        allRanks.filterNot(_ == tripRank)
      )
      pairSuit1 <- suitGen
      pairSuit2 <- Gen.oneOf(
        allSuits.filterNot(_ == pairSuit1)
      )
      kicker1Rank <- Gen.oneOf(
        allRanks.filterNot(Set(tripRank, pairRank).contains)
      )
      kicker1Suit <- suitGen
      kicker2Rank <- Gen.oneOf(
        allRanks.filterNot(Set(tripRank, pairRank, kicker1Rank).contains)
      )
      kicker2Suit <- suitGen
    } yield {
      orderCards(List(
        tripRank of tripSuit1, tripRank of tripSuit2, tripRank of tripSuit3,
        pairRank of pairSuit1, pairRank of pairSuit2,
        kicker1Rank of kicker1Suit, kicker2Rank of kicker2Suit,
      ))
    }
    gen.suchThat(cards => cards.size == 7 && cards.distinct.size == 7)
  }

  val fourOfAKindCardsGen: Gen[List[Card]] = {
    val gen = for {
      quadRank <- rankGen
      quadSuit1 <- suitGen
      quadSuit2 <- Gen.oneOf(
        allSuits.filterNot(Set(quadSuit1).contains)
      )
      quadSuit3 <- Gen.oneOf(
        allSuits.filterNot(Set(quadSuit1, quadSuit2).contains)
      )
      quadSuit4 <- Gen.oneOf(
        allSuits.filterNot(Set(quadSuit1, quadSuit2, quadSuit3).contains)
      )
      // no harm in adding pairs or trips here it'd still be a four-of-a-kind
      // no chance of a straight flush so no limits on the kickers
      kicker1Rank <- Gen.oneOf(
        allRanks.filterNot(Set(quadRank).contains)
      )
      kicker1Suit <- suitGen
      kicker2Rank <- Gen.oneOf(
        allRanks.filterNot(Set(quadRank).contains)
      )
      kicker2Suit <- suitGen
      kicker3Rank <- Gen.oneOf(
        allRanks.filterNot(Set(quadRank).contains)
      )
      kicker3Suit <- suitGen
    } yield {
      orderCards(List(
        quadRank of quadSuit1, quadRank of quadSuit2, quadRank of quadSuit3, quadRank of quadSuit4,
        kicker1Rank of kicker1Suit, kicker2Rank of kicker2Suit, kicker3Rank of kicker3Suit,
      ))
    }
    gen.suchThat(cards => cards.size == 7 && cards.distinct.size == 7)
  }

  val straightFlushCardsGen: Gen[List[Card]] = {
    val gen = for {
      (rank1, rank2, rank3, rank4, rank5) <- Gen.oneOf(
        (Ace, King, Queen, Jack, Ten),
        (King, Queen, Jack, Ten, Nine),
        (Queen, Jack, Ten, Nine, Eight),
        (Jack, Ten, Nine, Eight, Seven),
        (Ten, Nine, Eight, Seven, Six),
        (Nine, Eight, Seven, Six, Five),
        (Eight, Seven, Six, Five, Four),
        (Seven, Six, Five, Four, Three),
        (Six, Five, Four, Three, Two),
        (Five, Four, Three, Two, Ace),
      )
      flushSuit <- suitGen
      kicker1Rank <- Gen.oneOf(
        allRanks.filterNot(Set(rank1, rank2, rank3, rank4, rank5).contains)
      )
      kicker1Suit <- suitGen
      kicker2Rank <- Gen.oneOf(
        allRanks.filterNot(Set(rank1, rank2, rank3, rank4, rank5).contains)
      )
      kicker2Suit <- suitGen
    } yield {
      orderCards(List(
        rank1 of flushSuit, rank2 of flushSuit, rank3 of flushSuit, rank4 of flushSuit, rank5 of flushSuit,
        kicker1Rank of kicker1Suit, kicker2Rank of kicker2Suit
      ))
    }
    gen.suchThat(cards => cards.size == 7 && cards.distinct.size == 7)
  }

  // other hand generators
  val twoTripsGen: Gen[List[Card]] = {
    val gen = for {
      trip1Rank <- rankGen
      trip2Rank <- Gen.oneOf(
        allRanks.filterNot(Set(trip1Rank).contains)
      )
      kickerRank <- Gen.oneOf(
        allRanks.filterNot(Set(trip1Rank, trip2Rank).contains)
      )
      trip1Suit1 <- suitGen
      trip1Suit2 <- Gen.oneOf(
        allSuits.filterNot(Set(trip1Suit1).contains)
      )
      trip1Suit3 <- Gen.oneOf(
        allSuits.filterNot(Set(trip1Suit1, trip1Suit2).contains)
      )
      trip2Suit1 <- suitGen
      trip2Suit2 <- Gen.oneOf(
        allSuits.filterNot(Set(trip2Suit1).contains)
      )
      trip2Suit3 <- Gen.oneOf(
        allSuits.filterNot(Set(trip2Suit1, trip2Suit2).contains)
      )
      kickerSuit <- suitGen
    } yield {
      orderCards(List(
        trip1Rank of trip1Suit1, trip1Rank of trip1Suit2, trip1Rank of trip1Suit3,
        trip2Rank of trip2Suit1, trip2Rank of trip2Suit2, trip2Rank of trip2Suit3,
        kickerRank of kickerSuit
      ))
    }
    gen.suchThat(cards => cards.size == 7 && cards.distinct.size == 7)
  }

  val longStraightGenerator: Gen[List[Card]] = {
    val gen = for {
      (high, next1, next2, next3, next4, next5, low) <- Gen.oneOf(
        (Ace, King, Queen, Jack, Ten, Nine, Eight),
        (King, Queen, Jack, Ten, Nine, Eight, Seven),
        (Queen, Jack, Ten, Nine, Eight, Seven, Six),
        (Jack, Ten, Nine, Eight, Seven, Six, Five),
        (Ten, Nine, Eight, Seven, Six, Five, Four),
        (Nine, Eight, Seven, Six, Five, Four, Three),
        (Eight, Seven, Six, Five, Four, Three, Two),
        (Seven, Six, Five, Four, Three, Two, Ace),
      )
      suit1 <- suitGen
      suit2 <- suitGen
      suit3 <- suitGen
      suit4 <- suitGen
      suit5 <- Gen.oneOf(
        allSuits.filterNot(
          breakFlush(suit1, suit2, suit3, suit4).contains
        )
      )
      suit6 <- Gen.oneOf(
        allSuits.filterNot(
          breakFlush(suit1, suit2, suit3, suit4, suit5)
        )
      )
      suit7 <- Gen.oneOf(
        allSuits.filterNot(
          breakFlush(suit1, suit2, suit3, suit4, suit5, suit6)
        )
      )
    } yield {
      orderCards(List(
        high of suit1, next1 of suit2, next2 of suit3, next3 of suit4,
        next4 of suit5, next5 of suit6, low of suit7
      ))
    }
    gen.suchThat(cards => cards.size == 7 && cards.distinct.size == 7)
  }

  val longStraightFlushGenerator: Gen[List[Card]] = {
    val gen = for {
      (high, next1, next2, next3, next4, next5, low) <- Gen.oneOf(
        (Ace, King, Queen, Jack, Ten, Nine, Eight),
        (King, Queen, Jack, Ten, Nine, Eight, Seven),
        (Queen, Jack, Ten, Nine, Eight, Seven, Six),
        (Jack, Ten, Nine, Eight, Seven, Six, Five),
        (Ten, Nine, Eight, Seven, Six, Five, Four),
        (Nine, Eight, Seven, Six, Five, Four, Three),
        (Eight, Seven, Six, Five, Four, Three, Two),
        (Seven, Six, Five, Four, Three, Two, Ace),
      )
      suit <- suitGen
    } yield {
      orderCards(List(
        high of suit, next1 of suit, next2 of suit, next3 of suit,
        next4 of suit, next5 of suit, low of suit
      ))
    }
    gen.suchThat(cards => cards.size == 7 && cards.distinct.size == 7)
  }

  val aceLowStraightGenerator: Gen[List[Card]] = {
    val gen = for {
      suit1 <- suitGen
      suit2 <- suitGen
      suit3 <- suitGen
      suit4 <- suitGen
      suit5 <- Gen.oneOf(
        allSuits.filterNot(
          breakFlush(suit1, suit2, suit3, suit4).contains
        )
      )

      kicker1Rank <- Gen.oneOf(
        allRanks.filterNot(_ == Six)
      )
      kicker1Suit <- Gen.oneOf(
        allSuits.filterNot(
          breakFlush(suit1, suit2, suit3, suit4, suit5)
        )
      )
      kicker2Rank <- Gen.oneOf(
        allRanks.filterNot(_ == Six)
      )
      kicker2Suit <- Gen.oneOf(
        allSuits.filterNot(
          breakFlush(suit1, suit2, suit3, suit4, suit5, kicker1Suit)
        )
      )
    } yield {
      orderCards(List(
        Ace of suit1, Two of suit2, Three of suit3, Four of suit4, Five of suit5,
        kicker1Rank of kicker1Suit, kicker2Rank of kicker2Suit
      ))
    }
    gen.suchThat(cards => cards.size == 7 && cards.distinct.size == 7)
  }

  val aceLowStraightFlushGenerator: Gen[List[Card]] = {
    val gen = for {
      suit <- suitGen
      kicker1Rank <- Gen.oneOf(
        allRanks.filterNot(_ == Six)
      )
      kicker1Suit <- suitGen
      kicker2Rank <- Gen.oneOf(
        allRanks.filterNot(_ == Six)
      )
      kicker2Suit <- suitGen
    } yield {
      orderCards(List(
        Ace of suit, Two of suit, Three of suit, Four of suit, Five of suit,
        kicker1Rank of kicker1Suit, kicker2Rank of kicker2Suit
      ))
    }
    gen.suchThat(cards => cards.size == 7 && cards.distinct.size == 7)
  }

  private def orderCards(cards: List[Card]): List[Card] = {
    cards.sortBy(cardOrd(acesHigh = true)).reverse
  }

  /**
   * Return rank(s) that would make a straight when combined the provided four.
   */
  def breakStraight(rank1: Rank, rank2: Rank, rank3: Rank, rank4: Rank, otherRanks: Rank*): Set[Rank] = {
    def check4(ranks: List[Rank]): Set[Rank] = {
      ranks match {
        // write cases out rather than try to be clever with ranks
        case Ace :: King :: Queen :: Jack :: _ =>
          Set(Ten)
        case Ace :: Queen :: Jack :: Ten :: _ =>
          Set(King)
        case Ace :: King :: Jack :: Ten :: _ =>
          Set(Queen)
        case Ace :: King :: Queen :: Ten :: _ =>
          Set(Jack)

        case King :: Queen :: Jack :: Ten :: _ =>
          Set(Ace, Nine)
        case King :: Jack :: Ten :: Nine :: _ =>
          Set(Queen)
        case King :: Queen :: Ten :: Nine :: _ =>
          Set(Jack)
        case King :: Queen :: Jack :: Nine :: _ =>
          Set(Ten)

        case Queen :: Jack :: Ten :: Nine :: _ =>
          Set(King, Eight)
        case Queen :: Ten :: Nine :: Eight :: _ =>
          Set(Jack)
        case Queen :: Jack :: Nine :: Eight :: _ =>
          Set(Ten)
        case Queen :: Jack :: Ten :: Eight :: _ =>
          Set(Nine)

        case Jack :: Ten :: Nine :: Eight :: _ =>
          Set(Queen, Seven)
        case Jack :: Nine :: Eight :: Seven :: _ =>
          Set(Ten)
        case Jack :: Ten :: Eight :: Seven :: _ =>
          Set(Nine)
        case Jack :: Ten :: Nine :: Seven :: _ =>
          Set(Eight)

        case Ten :: Nine :: Eight :: Seven :: _ =>
          Set(Jack, Six)
        case Ten :: Eight :: Seven :: Six :: _ =>
          Set(Nine)
        case Ten :: Nine :: Seven :: Six :: _ =>
          Set(Eight)
        case Ten :: Nine :: Eight :: Six :: _ =>
          Set(Seven)

        case Nine :: Eight :: Seven :: Six :: _ =>
          Set(Ten, Five)
        case Nine :: Seven :: Six :: Five :: _ =>
          Set(Eight)
        case Nine :: Eight :: Six :: Five :: _ =>
          Set(Seven)
        case Nine :: Eight :: Seven :: Five :: _ =>
          Set(Six)

        case Eight :: Seven :: Six :: Five :: _ =>
          Set(Nine, Four)
        case Eight :: Six :: Five :: Four :: _ =>
          Set(Seven)
        case Eight :: Seven :: Five :: Four :: _ =>
          Set(Six)
        case Eight :: Seven :: Six :: Four :: _ =>
          Set(Five)

        case Seven :: Six :: Five :: Four :: _ =>
          Set(Eight, Three)
        case Seven :: Five :: Four :: Three :: _ =>
          Set(Six)
        case Seven :: Six :: Four :: Three :: _ =>
          Set(Five)
        case Seven :: Six :: Five :: Three :: _ =>
          Set(Four)

        case Six :: Five :: Four :: Three :: _ =>
          Set(Seven, Two)
        case Six :: Four :: Three :: Two :: _ =>
          Set(Five )
        case Six :: Five :: Three :: Two :: _ =>
          Set(Four)
        case Six :: Five :: Four :: Two :: _ =>
          Set(Three)

        case Five :: Four :: Three :: Two :: _ =>
          Set(Six, Ace)
        case Five :: Three :: Two :: Ace :: _ =>
          Set(Four)
        case Five :: Four :: Two :: Ace :: _ =>
          Set(Three)
        case Five :: Four :: Three :: Ace :: _ =>
          Set(Two)

        case Four :: Three :: Two :: Ace :: _ =>
          Set(Five)

        case _ => Set.empty
      }
    }

    val aceNormalisedRanks = (Seq(rank1, rank2, rank3, rank4) ++ otherRanks).toList
      .distinct.sortBy(rankOrd(acesHigh = true)).reverse match {
      case ranks @ Ace :: _ =>
        ranks :+ Ace
      case ranks =>
        ranks
    }
    aceNormalisedRanks
      .sliding(4)
      .filterNot(_.length != 4)
      .foldLeft[Set[Rank]](Set.empty) { (banned, ranks) =>
        banned.union(check4(ranks))
      }
  }

  def breakFlush(suits: Suit*): Set[Suit] = {
    suits
      .groupBy(identity)
      .view.mapValues(_.size)
      .filter { case (_, count) => count >= 4 }
      .keys
      .toSet
  }
}
