package io.adamnfish.pokerdot.logic

import io.adamnfish.pokerdot.models.{Ace, Card, Clubs, Diamonds, Eight, Five, Flush, Four, FourOfAKind, FullHouse, Hand, Hearts, HighCard, Hole, Jack, King, Nine, Pair, Player, Queen, Rank, Round, Seven, Six, Spades, Straight, StraightFlush, Suit, Ten, Three, ThreeOfAKind, Two, TwoPair}


object PokerHands {
  /**
   * Calculate the winnings for each player.
   *
   * Typically this is a single winner taking everything, but it is more
   * complex in some situations:
   *   - split pots
   *   - rounds where one or more players are all-in
   */
  def winnings(round: Round, players: List[Player]): List[(Player, Hand, Int)] = {
    // order hands by strength - ensuring ties are considered at the same time
    // from strongest to weakest, distribute winning shares of pot until pot is empty

    val playerHands =
      for {
        player <- players
        hole <- player.hole
      } yield (player, bestHand(round, hole))

    val playerHandsByStrength = playerHands
      // equal strengths are clumped together
      .groupBy { case (_, hand) => handOrd(hand) }
      .toList
      // and then sorted by strength
      .sortBy { case (handStrength, _) => handStrength }
      .map { case (_, playerHands) => playerHands }
      .reverse

    val (results, _) = playerHandsByStrength.foldLeft[(List[(Player, Hand, Int)], List[Player])]((Nil, players)) {
      // we keep track of calculated winnings and the current state of the pot (via paidPlayers)
      // playerHands is all the hands of with the next strength (more than 1 of there is a tie)
      case ((winningPlayers, paidPlayers), playerHands) =>
        playerHands match {
          case (winningPlayer, winningHand) :: Nil =>
            val (winningsHere, updatedPaidPlayers) =
              paidPlayers.foldRight[(Int, List[Player])]((0, Nil)) { case (paidPlayer, (winnings, newPaidPlayers)) =>
                val winningsFromThisPlayer = math.min(paidPlayer.pot, winningPlayer.pot)
                (winnings + winningsFromThisPlayer, paidPlayer.copy(pot = paidPlayer.pot - winningsFromThisPlayer) :: newPaidPlayers)
              }

            (
              (winningPlayer, winningHand, winningsHere) :: winningPlayers,
              updatedPaidPlayers
            )
          case tiedWinners =>
            val winnersByPotContribution = tiedWinners
              // grouped by how much they contributed to the pot
              // less than the max is a side-pot, and needs to be addressed first
              .groupBy(_._1.pot)
              .toList
              .sortBy(_._1)
              .map(_._2)
              // no reverse - we want the smallest contribution first (side-pot)

            ???
        }
    }
    results
  }

  /**
   * Fetch the best hand for a player, which will be something between "high card" and "straight flush".
   *
   * Note that "royal flush" is a special case of "straight flush" (high card is an Ace).
   */
  def bestHand(round: Round, hole: Hole): Hand = {
    val sevenCards = List(
      round.flop1,
      round.flop2,
      round.flop3,
      round.turn,
      round.river,
      hole.card1,
      hole.card2,
    ).sortBy(cardOrd(acesHigh = true)).reverse

    // We'll need to call these over and over, let's do it once here
    val duplicates = findDuplicates(sevenCards)
    val duplicateSuits = findDuplicateSuits(sevenCards)

    // check for the strongest first and fallback to lesser hands
    straightFlush(sevenCards, duplicateSuits) orElse
      fourOfAKind(sevenCards, duplicates)     orElse
      fullHouse(sevenCards, duplicates)       orElse
      flush(sevenCards, duplicateSuits)       orElse
      straight(sevenCards)                    orElse
      threeOfAKind(sevenCards, duplicates)    orElse
      twoPair(sevenCards, duplicates)         orElse
      pair(sevenCards, duplicates)            getOrElse
        highCard(sevenCards)
  }

  def findDuplicates(cards: List[Card]): List[(Rank, List[Card])] = {
    cards
      .groupBy(_.rank).toList
      .sortBy { case (rank, _) =>
        rankOrd(acesHigh = true)(rank)
      }
      .reverse
  }

  def findDuplicateSuits(cards: List[Card]): Map[Suit, List[Card]] = {
    cards.groupBy(_.suit)
  }

  // Deal with each of the poker hands
  // each of these assumes:
  // - the cards are already sorted by rank high -> low
  // - duplicate groupings are sorted by rank high -> low

  /**
   * Returns the highest 5 cards of the 7, which is always possible.
   *
   * No checks for other hands, if highCard is called it is assumed that a better hand does not exist.
   */
  def highCard(sevenCards: List[Card]): HighCard = {
    sevenCards match {
      case highCard :: kicker1 :: kicker2 :: kicker3 :: kicker4 :: _ =>
        HighCard(highCard, kicker1, kicker2, kicker3, kicker4)
      case _ =>
        // unreachable code, there are 7 items in this list
        throw new RuntimeException(s"Unreachable code. HighCard was passed fewer than 7 cards $sevenCards")
    }
  }

  def pair(sevenCards: List[Card], duplicates: List[(Rank, List[Card])]): Option[Pair] = {
    duplicates.filter { case (_, cards) =>
      cards.size == 2
    } match {
      // Note: If there is more than one pair this is not a "pair hand" so we'll reject it
      case (rank, pair1 :: pair2 :: Nil) :: Nil =>
        sevenCards.filterNot(card => card == pair1 || card == pair2) match {
          case kicker1 :: kicker2 :: kicker3 :: _ =>
            Some(
              Pair(pair1, pair2, kicker1, kicker2, kicker3)
            )
          case _ =>
            throw new RuntimeException(s"Unreachable code. Pair of $rank left fewer than 3 cards from 7 $sevenCards")
        }
      case _ =>
        // no pairs
        None
    }
  }

  def twoPair(sevenCards: List[Card], duplicates: List[(Rank, List[Card])]): Option[TwoPair] = {
    duplicates.filter { case (_, cards) =>
      cards.size == 2
    } match {
      // Note: there could be more than two pairs in 7 cards, we'll take the strongest 2
      case (upRank, up1 :: up2 :: Nil) :: (downRank, down1 :: down2 :: Nil) :: _ =>
        sevenCards.filterNot(card => card == up1 || card == up2 || card == down1 || card == down2) match {
          case kicker :: _ =>
            Some(
              TwoPair(up1, up2, down1, down2, kicker)
            )
          case _ =>
            throw new RuntimeException(s"Unreachable code. Pairs of $upRank, $downRank left fewer than 1 card from 7 $sevenCards")
        }
      case _ =>
        // fewer than 2 pairs
        None
    }
  }

  def threeOfAKind(sevenCards: List[Card], duplicates: List[(Rank, List[Card])]): Option[ThreeOfAKind] = {
    duplicates.filter { case (_, cards) =>
      cards.size == 3
    } match {
      // Note: there could be two "three of a kinds" in 7 cards, we'll take the highest
      case (rank, trip1 :: trip2 :: trip3 :: Nil) :: _ =>
        sevenCards.filterNot(card => card.rank == rank) match {
          case kicker1 :: kicker2 :: _ =>
            Some(
              ThreeOfAKind(trip1, trip2, trip3, kicker1, kicker2)
            )
          case _ =>
            throw new RuntimeException(s"Unreachable code. Three of a kind of $rank left fewer than 2 cards from 7 $sevenCards")
        }
      case _ =>
        // no three of a kinds present
        None
    }
  }

  /**
   * This logic is re-used from Straight flush, so it may be passed as few as 5 cards
   */
  def straight(upToSevenCards: List[Card]): Option[Straight] = {
    def check5(cards: List[Card]): Option[Straight] = {
      // check each case explicitly rather than try and be clever with ranks
      cards match {
        case (c1 @ Card(Ace, _)) :: (c2 @ Card(King, _)) :: (c3 @ Card(Queen, _)) :: (c4 @ Card(Jack, _)) :: (c5 @ Card(Ten, _)) :: _ =>
          Some(Straight(c1, c2, c3, c4, c5))
        case (c1 @ Card(King, _)) :: (c2 @ Card(Queen, _)) :: (c3 @ Card(Jack, _)) :: (c4 @ Card(Ten, _)) :: (c5 @ Card(Nine, _)) :: _ =>
          Some(Straight(c1, c2, c3, c4, c5))
        case (c1 @ Card(Queen, _)) :: (c2 @ Card(Jack, _)) :: (c3 @ Card(Ten, _)) :: (c4 @ Card(Nine, _)) :: (c5 @ Card(Eight, _)) :: _ =>
          Some(Straight(c1, c2, c3, c4, c5))
        case (c1 @ Card(Jack, _)) :: (c2 @ Card(Ten, _)) :: (c3 @ Card(Nine, _)) :: (c4 @ Card(Eight, _)) :: (c5 @ Card(Seven, _)) :: _ =>
          Some(Straight(c1, c2, c3, c4, c5))
        case (c1 @ Card(Ten, _)) :: (c2 @ Card(Nine, _)) :: (c3 @ Card(Eight, _)) :: (c4 @ Card(Seven, _)) :: (c5 @ Card(Six, _)) :: _ =>
          Some(Straight(c1, c2, c3, c4, c5))
        case (c1 @ Card(Nine, _)) :: (c2 @ Card(Eight, _)) :: (c3 @ Card(Seven, _)) :: (c4 @ Card(Six, _)) :: (c5 @ Card(Five, _)) :: _ =>
          Some(Straight(c1, c2, c3, c4, c5))
        case (c1 @ Card(Eight, _)) :: (c2 @ Card(Seven, _)) :: (c3 @ Card(Six, _)) :: (c4 @ Card(Five, _)) :: (c5 @ Card(Four, _)) :: _ =>
          Some(Straight(c1, c2, c3, c4, c5))
        case (c1 @ Card(Seven, _)) :: (c2 @ Card(Six, _)) :: (c3 @ Card(Five, _)) :: (c4 @ Card(Four, _)) :: (c5 @ Card(Three, _)) :: _ =>
          Some(Straight(c1, c2, c3, c4, c5))
        case (c1 @ Card(Six, _)) :: (c2 @ Card(Five, _)) :: (c3 @ Card(Four, _)) :: (c4 @ Card(Three, _)) :: (c5 @ Card(Two, _)) :: _ =>
          Some(Straight(c1, c2, c3, c4, c5))
        // Ace-low straight so Ace goes at the end
        case (c5 @ Card(Ace, _)) :: (c1 @ Card(Five, _)) :: (c2 @ Card(Four, _)) :: (c3 @ Card(Three, _)) :: (c4 @ Card(Two, _)) :: _ =>
          Some(Straight(c1, c2, c3, c4, c5))
        case _ =>
          None
      }
    }
    // run through each set of 5 cards searching for straights
    // first one found will be highest
    upToSevenCards.sliding(5).filterNot(_.length < 5).foldLeft[Option[Straight]](None) { (hand, cards) =>
      hand orElse check5(cards)
    }
  }

  def flush(sevenCards: List[Card], duplicateSuits: Map[Suit, List[Card]]): Option[Flush] = {
    duplicateSuits.find { case (_, cards) =>
      cards.length >= 5
    }.map {
      case (_, high :: next1 :: next2 :: next3 :: low :: _) =>
        Flush(high, next1, next2, next3, low)
      case (suit, _) =>
        throw new RuntimeException(s"Unreachable code: flush of $suit did not have at least 5 cards $sevenCards")
    }
  }

  def fullHouse(sevenCards: List[Card], duplicates: List[(Rank, List[Card])]): Option[FullHouse] = {
    duplicates.filter { case (_, cards) =>
      // while it's possible a full house exists from a duplicate of  >3 cards,
      // four-of-a-kind is a stronger hand so we don't need to consider that case
      cards.size == 3
    } match {
      // if two triplets exist either could make a full house - we only need to consider the strongest
      case (overRank, trip1 :: trip2 :: trip3 :: Nil) :: _ =>
        val remainingDuplicates = sevenCards.filterNot(card => card.rank == overRank)
          .groupBy(_.rank).toList
          .sortBy { case (rank, _) =>
            rankOrd(acesHigh = true)(rank)
          }
          .reverse
        remainingDuplicates.filter { case (_, cards) =>
          // we could make a full house from two triplets, so need to consider more than just `== 2` here
          cards.size >= 2
        } match {
          case (_, pair1 :: pair2 :: _) :: _ =>
            Some(
              FullHouse(trip1, trip2, trip3, pair1, pair2)
            )
          case _ =>
            // we found a triplet, but no pairs after that
            None
        }
        None
      case _ =>
        // no triplets means no full house
        None
    }
  }

  def fourOfAKind(sevenCards: List[Card], duplicates: List[(Rank, List[Card])]): Option[FourOfAKind] = {
    duplicates.filter { case (_, cards) =>
      cards.size == 4
    } match {
      case (rank, quad1 :: quad2 :: quad3 :: quad4 :: Nil) :: _ =>
        sevenCards.filterNot(Set(quad1, quad2, quad3, quad4).contains) match {
          case kicker :: _ =>
            Some(
              FourOfAKind(quad1, quad2, quad3, quad4, kicker)
            )
          case _ =>
            throw new RuntimeException(s"Unreachable code. Four of a kind of $rank left fewer than 1 card from 7 $sevenCards")
        }
      case _ =>
        // no three of a kinds present
        None
    }
  }

  def straightFlush(sevenCards: List[Card], duplicateSuits: Map[Suit, List[Card]]): Option[StraightFlush] = {
    for {
      (_, flushCards) <-
        duplicateSuits.find { case (_, cards) =>
          cards.length >= 5
        }
      // if we can make a straight from the flush cards, we have a straight flush
      straight <- straight(flushCards)
    } yield {
      StraightFlush(
        high  = straight.high,
        next1 = straight.next1,
        next2 = straight.next2,
        next3 = straight.next3,
        low   = straight.low,
      )
    }
  }

  // hand utils

  /**
   * Hands are ordered by "hand type" and then card ranks to tie-break.
   */
  def handOrd(hand: Hand): (Int, Int, Int, Int, Int, Int) = {
    val aceHighRankOrd = rankOrd(acesHigh = true) _
    hand match {
      case HighCard(highCard, kicker1, kicker2, kicker3, kicker4) =>
        (0,
          aceHighRankOrd(highCard.rank),
          aceHighRankOrd(kicker1.rank),
          aceHighRankOrd(kicker2.rank),
          aceHighRankOrd(kicker3.rank),
          aceHighRankOrd(kicker4.rank),
        )
      case Pair(pair1, pair2, kicker1, kicker2, kicker3) =>
        (1,
          aceHighRankOrd(pair1.rank),
          aceHighRankOrd(pair2.rank),
          aceHighRankOrd(kicker1.rank),
          aceHighRankOrd(kicker2.rank),
          aceHighRankOrd(kicker3.rank),
        )
      case TwoPair(up1, up2, down1, down2, kicker) =>
        (2,
          aceHighRankOrd(up1.rank),
          aceHighRankOrd(up2.rank),
          aceHighRankOrd(down1.rank),
          aceHighRankOrd(down2.rank),
          aceHighRankOrd(kicker.rank),
        )
      case ThreeOfAKind(trip1, trip2, trip3, kicker1, kicker2) =>
        (3,
          aceHighRankOrd(trip1.rank),
          aceHighRankOrd(trip2.rank),
          aceHighRankOrd(trip3.rank),
          aceHighRankOrd(kicker1.rank),
          aceHighRankOrd(kicker2.rank),
        )
      case Straight(high, next1, next2, next3, low) =>
        // aces can be low in a straight
        val lowCardOrd =
          if (low.rank == Ace) rankOrd(acesHigh = false)(low.rank)
          else aceHighRankOrd(low.rank)
        (4,
          aceHighRankOrd(high.rank),
          aceHighRankOrd(next1.rank),
          aceHighRankOrd(next2.rank),
          aceHighRankOrd(next3.rank),
          lowCardOrd,
        )
      case Flush(high, next1, next2, next3, low) =>
        (5,
          aceHighRankOrd(high.rank),
          aceHighRankOrd(next1.rank),
          aceHighRankOrd(next2.rank),
          aceHighRankOrd(next3.rank),
          aceHighRankOrd(low.rank),
        )
      case FullHouse(trip1, trip2, trip3, pair1, pair2) =>
        (6,
          aceHighRankOrd(trip1.rank),
          aceHighRankOrd(trip2.rank),
          aceHighRankOrd(trip3.rank),
          aceHighRankOrd(pair1.rank),
          aceHighRankOrd(pair2.rank),
        )
      case FourOfAKind(quad1, quad2, quad3, quad4, kicker) =>
        (7,
          aceHighRankOrd(quad1.rank),
          aceHighRankOrd(quad2.rank),
          aceHighRankOrd(quad3.rank),
          aceHighRankOrd(quad4.rank),
          aceHighRankOrd(kicker.rank),
        )
      case StraightFlush(high, next1, next2, next3, low) =>
        // aces can be low in a straight
        val lowCardOrd =
          if (low.rank == Ace) rankOrd(acesHigh = false)(low.rank)
          else aceHighRankOrd(low.rank)
        (8,
          aceHighRankOrd(high.rank),
          aceHighRankOrd(next1.rank),
          aceHighRankOrd(next2.rank),
          aceHighRankOrd(next3.rank),
          lowCardOrd,
        )
    }
  }

  // Card utils

  def cardOrd(acesHigh: Boolean)(card: Card): (Int, Int) = {
    (rankOrd(acesHigh)(card.rank), suitOrd(card.suit))
  }

  def rankOrd(acesHigh: Boolean)(rank: Rank): Int = {
    rank match {
      case Two    => 2
      case Three  => 3
      case Four   => 4
      case Five   => 5
      case Six    => 6
      case Seven  => 7
      case Eight  => 8
      case Nine   => 9
      case Ten    => 10
      case Jack   => 11
      case Queen  => 12
      case King   => 13
      case Ace    =>
        if (acesHigh) 14
        else 1
    }
  }

  def suitOrd(suit: Suit): Int = {
    suit match {
      case Clubs    => 0
      case Diamonds => 1
      case Spades   => 2
      case Hearts   => 3
    }
  }
}
