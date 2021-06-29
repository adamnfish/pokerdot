module Views.Generators exposing (cardGen, cardsGen, flopRoundGen, flushHandGen, fourOfAKindHandGen, fullHouseHandGen, highCardHandGen, holeGen, nameGen, pairHandGen, riverRoundGen, royalFlushHandGen, straightFlushHandGen, straightHandGen, threeOfAKindHandGen, turnRoundGen, twoPairHandGen)

import Model exposing (Card, Hand(..), Rank(..), Round(..), Suit(..))
import Random exposing (Generator)
import Random.Char
import Random.String


suitGen : Generator Suit
suitGen =
    Random.uniform Clubs [ Diamonds, Hearts, Spades ]


rankGen : Generator Rank
rankGen =
    Random.uniform Two [ Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace ]


cardGen : Generator Card
cardGen =
    Random.map2
        (\rank suit -> { rank = rank, suit = suit })
        rankGen
        suitGen


cardsGen : Int -> Generator (List Card)
cardsGen size =
    Random.list size cardGen


flopRoundGen : Generator Round
flopRoundGen =
    Random.map3
        FlopRound
        cardGen
        cardGen
        cardGen


turnRoundGen : Generator Round
turnRoundGen =
    Random.map4
        TurnRound
        cardGen
        cardGen
        cardGen
        cardGen


riverRoundGen : Generator Round
riverRoundGen =
    Random.map5
        RiverRound
        cardGen
        cardGen
        cardGen
        cardGen
        cardGen


holeGen : Generator ( Card, Card )
holeGen =
    Random.pair cardGen cardGen


nameGen : Generator String
nameGen =
    Random.int 5 15
        |> Random.andThen
            (\length -> Random.String.string length Random.Char.english)



-- these aren't real generators, but good enough for testing layouts


highCardHandGen : Generator Hand
highCardHandGen =
    Random.map5 HighCard cardGen cardGen cardGen cardGen cardGen


pairHandGen : Generator Hand
pairHandGen =
    Random.map4
        (\c1 -> Pair { c1 | suit = Clubs } { c1 | suit = Spades })
        cardGen
        cardGen
        cardGen
        cardGen


twoPairHandGen : Generator Hand
twoPairHandGen =
    Random.map3
        (\c1 c2 -> TwoPair { c1 | suit = Clubs } { c1 | suit = Spades } { c2 | suit = Diamonds } { c2 | suit = Diamonds })
        cardGen
        cardGen
        cardGen


threeOfAKindHandGen : Generator Hand
threeOfAKindHandGen =
    Random.map3
        (\c1 -> ThreeOfAKind { c1 | suit = Clubs } { c1 | suit = Spades } { c1 | suit = Diamonds })
        cardGen
        cardGen
        cardGen


straightRanksGen : Generator { one : Rank, two : Rank, three : Rank, four : Rank, five : Rank }
straightRanksGen =
    Random.uniform
        { one = Ace, two = Two, three = Three, four = Four, five = Five }
        [ { one = Two, two = Three, three = Four, four = Five, five = Six }
        , { one = Three, two = Four, three = Five, four = Six, five = Seven }
        , { one = Three, two = Four, three = Five, four = Six, five = Seven }
        , { one = Four, two = Five, three = Six, four = Seven, five = Eight }
        , { one = Five, two = Six, three = Seven, four = Eight, five = Nine }
        , { one = Six, two = Seven, three = Eight, four = Nine, five = Ten }
        , { one = Seven, two = Eight, three = Nine, four = Ten, five = Jack }
        , { one = Eight, two = Nine, three = Ten, four = Jack, five = Queen }
        , { one = Nine, two = Ten, three = Jack, four = Queen, five = King }
        , { one = Ten, two = Jack, three = Queen, four = King, five = Ace }
        ]


straightHandGen : Generator Hand
straightHandGen =
    Random.andThen
        (\ranks ->
            Random.map5
                (\s1 s2 s3 s4 s5 ->
                    Straight
                        { rank = ranks.one, suit = s1 }
                        { rank = ranks.two, suit = s2 }
                        { rank = ranks.three, suit = s3 }
                        { rank = ranks.four, suit = s4 }
                        { rank = ranks.five, suit = s5 }
                )
                suitGen
                suitGen
                suitGen
                suitGen
                suitGen
        )
        straightRanksGen


flushHandGen : Generator Hand
flushHandGen =
    Random.andThen
        (\suit ->
            Random.map5
                (\r1 r2 r3 r4 r5 ->
                    Flush
                        { suit = suit, rank = r1 }
                        { suit = suit, rank = r2 }
                        { suit = suit, rank = r3 }
                        { suit = suit, rank = r4 }
                        { suit = suit, rank = r5 }
                )
                rankGen
                rankGen
                rankGen
                rankGen
                rankGen
        )
        suitGen


fullHouseHandGen : Generator Hand
fullHouseHandGen =
    let
        ranksGen =
            Random.map2
                (\r1 r2 -> ( r1, r2 ))
                rankGen
                rankGen
    in
    Random.andThen
        (\( r1, r2 ) ->
            Random.map5
                (\s1 s2 s3 s4 s5 ->
                    FullHouse
                        { suit = s1, rank = r1 }
                        { suit = s2, rank = r1 }
                        { suit = s3, rank = r1 }
                        { suit = s4, rank = r2 }
                        { suit = s5, rank = r2 }
                )
                suitGen
                suitGen
                suitGen
                suitGen
                suitGen
        )
        ranksGen


fourOfAKindHandGen : Generator Hand
fourOfAKindHandGen =
    Random.map2
        (\r1 ->
            FourOfAKind
                { suit = Clubs, rank = r1 }
                { suit = Diamonds, rank = r1 }
                { suit = Hearts, rank = r1 }
                { suit = Spades, rank = r1 }
        )
        rankGen
        cardGen


straightFlushHandGen : Generator Hand
straightFlushHandGen =
    Random.map2
        (\suit { one, two, three, four, five } ->
            StraightFlush
                { suit = suit, rank = one }
                { suit = suit, rank = two }
                { suit = suit, rank = three }
                { suit = suit, rank = four }
                { suit = suit, rank = five }
        )
        suitGen
        straightRanksGen


royalFlushHandGen : Generator Hand
royalFlushHandGen =
    Random.map
        (\suit ->
            StraightFlush
                { suit = suit, rank = Ten }
                { suit = suit, rank = Jack }
                { suit = suit, rank = Queen }
                { suit = suit, rank = King }
                { suit = suit, rank = Ace }
        )
        suitGen
