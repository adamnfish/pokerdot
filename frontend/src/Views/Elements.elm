module Views.Elements exposing (cardUi, communityCardsUi, dotContainer, handUi, pdButton, pdButtonSmall, pdTab, pdText, selfUi, tableUi, uiElements, zWidths)

import Browser.Dom exposing (Viewport)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (labelAbove, labelLeft)
import Element.Region as Region
import Model exposing (Card, Game, Hand(..), Msg(..), Player, Round(..), Self)
import Random
import Random.Extra
import Views.Generators exposing (..)


pdButton : Msg -> List String -> Element Msg
pdButton msg lines =
    internalButton 100 msg lines


pdButtonSmall : Msg -> List String -> Element Msg
pdButtonSmall msg lines =
    internalButton 80 msg lines


pdTab : Msg -> String -> Element Msg
pdTab msg label =
    Input.button
        [ Border.rounded 1
        , Border.solid
        , Border.width 2
        , Border.color <| rgb255 60 60 60
        , Border.shadow
            { offset = ( 1, 1 )
            , size = 1
            , blur = 1
            , color = rgb255 120 120 120
            }
        , focused
            [ Background.color <| rgb255 220 220 230
            , Border.color <| rgb255 120 120 240
            ]
        ]
        { onPress = Just msg
        , label = text label
        }


internalButton : Int -> Msg -> List String -> Element Msg
internalButton diameter msg lines =
    Input.button
        [ Border.rounded (diameter // 2)
        , width <| px diameter
        , height <| px diameter
        , Border.solid
        , Border.width 2
        , Border.color <| rgb255 60 60 60
        , Border.shadow
            { offset = ( 1, 1 )
            , size = 1
            , blur = 1
            , color = rgb255 120 120 120
            }
        , focused
            [ Background.color <| rgb255 220 220 230
            , Border.color <| rgb255 120 120 240
            ]
        ]
        { onPress = Just msg
        , label =
            column
                [ width fill ]
            <|
                List.map
                    (\s ->
                        el
                            [ centerX ]
                        <|
                            text (s ++ " ")
                    )
                    lines
        }


pdText : (String -> msg) -> String -> String -> Element msg
pdText msg value labelStr =
    Input.text
        [ Font.alignLeft
        , paddingXY 10 8
        , Border.solid
        , Border.width 2
        , Border.color <| rgb255 60 60 60
        , Border.widthEach { zWidths | bottom = 2 }
        , Border.rounded 0
        , Background.color <| rgb255 200 200 200
        , focused
            [ Background.color <| rgb255 220 220 230
            , Border.color <| rgb255 120 120 240
            ]
        ]
        { onChange = msg
        , text = value
        , placeholder = Nothing
        , label =
            Input.labelAbove
                [ alignLeft ]
            <|
                text labelStr
        }


zWidths : { bottom : Int, left : Int, right : Int, top : Int }
zWidths =
    { bottom = 0
    , left = 0
    , right = 0
    , top = 0
    }


dotContainer : Viewport -> Int -> Element Msg -> Element Msg
dotContainer viewport radius content =
    el
        [ width fill
        ]
    <|
        el
            [ width <| px radius
            , height <| px radius
            , centerX
            , Background.color <| rgb255 180 180 230
            , Border.rounded radius
            ]
        <|
            el
                [ width <|
                    maximum (radius - 24) <|
                        px <|
                            round (viewport.viewport.width - 24)
                , height <| px radius
                , centerX
                ]
                content


tableUi : Game -> Element Msg
tableUi game =
    let
        pot =
            List.sum <| List.map .pot game.players

        seat : Player -> Element Msg
        seat player =
            row
                [ width fill
                , spacing 8
                ]
                [ text player.screenName
                , text <| String.fromInt player.stack
                , text <| String.fromInt player.bet
                ]
    in
    column
        [ width fill
        ]
        [ column
            [ width fill ]
          <|
            List.map seat game.players
        , text <| "pot: " ++ String.fromInt pot
        ]


selfUi : Bool -> Self -> Element Msg
selfUi isPeeking self =
    if self.busted then
        row
            [ width fill ]
            [ text self.screenName ]

    else
        row
            [ width fill ]
            [ text self.screenName
            , text " "
            , case self.hole of
                Nothing ->
                    text " - "

                Just ( card1, card2 ) ->
                    row
                        []
                    <|
                        if isPeeking then
                            List.intersperse (text " ") <| List.map cardUi [ card1, card2 ]

                        else
                            [ text " - ", text " - " ]
            , pdTab TogglePeek <|
                if isPeeking then
                    "stop looking at hand"

                else
                    "look at hand"
            ]


cardUi : Card -> Element Msg
cardUi card =
    let
        rank =
            case card.rank of
                Model.Two ->
                    text "2"

                Model.Three ->
                    text "3"

                Model.Four ->
                    text "4"

                Model.Five ->
                    text "5"

                Model.Six ->
                    text "6"

                Model.Seven ->
                    text "7"

                Model.Eight ->
                    text "8"

                Model.Nine ->
                    text "9"

                Model.Ten ->
                    text "10"

                Model.Jack ->
                    text "J"

                Model.Queen ->
                    text "Q"

                Model.King ->
                    text "K"

                Model.Ace ->
                    text "A"

        suit =
            case card.suit of
                Model.Clubs ->
                    text "♣"

                Model.Diamonds ->
                    text "♦"

                Model.Spades ->
                    text "♠"

                Model.Hearts ->
                    text "♥"
    in
    row [] [ rank, suit ]


handUi : Hand -> Element Msg
handUi hand =
    case hand of
        HighCard c1 c2 c3 c4 c5 ->
            column
                []
                [ text "High card"
                , row
                    []
                    [ cardUi c1
                    , cardUi c2
                    , cardUi c3
                    , cardUi c4
                    , cardUi c5
                    ]
                ]

        Pair p1 p2 k1 k2 k3 ->
            column
                []
                [ text "Pair"
                , row
                    []
                    [ cardUi p1
                    , cardUi p2
                    , text " | "
                    , cardUi k1
                    , cardUi k2
                    , cardUi k3
                    ]
                ]

        TwoPair p11 p12 p21 p22 k ->
            column
                []
                [ text "Two pair"
                , row
                    []
                    [ cardUi p11
                    , cardUi p12
                    , text " | "
                    , cardUi p21
                    , cardUi p22
                    , text " | "
                    , cardUi k
                    ]
                ]

        ThreeOfAKind t1 t2 t3 k1 k2 ->
            column
                []
                [ text "Three of a kind"
                , row
                    []
                    [ cardUi t1
                    , cardUi t2
                    , cardUi t3
                    , text " | "
                    , cardUi k1
                    , cardUi k2
                    ]
                ]

        Straight c1 c2 c3 c4 c5 ->
            column
                []
                [ text "Straight"
                , row
                    []
                    [ cardUi c1
                    , cardUi c2
                    , cardUi c3
                    , cardUi c4
                    , cardUi c5
                    ]
                ]

        Flush c1 c2 c3 c4 c5 ->
            column
                []
                [ text "Flush"
                , row
                    []
                    [ cardUi c1
                    , cardUi c2
                    , cardUi c3
                    , cardUi c4
                    , cardUi c5
                    ]
                ]

        FullHouse t1 t2 t3 p1 p2 ->
            column
                []
                [ text "Full house"
                , row
                    []
                    [ cardUi t1
                    , cardUi t2
                    , cardUi t3
                    , text " | "
                    , cardUi p1
                    , cardUi p2
                    ]
                ]

        FourOfAKind q1 q2 q3 q4 k ->
            column
                []
                [ text "Four of a kind"
                , row
                    []
                    [ cardUi q1
                    , cardUi q2
                    , cardUi q3
                    , cardUi q4
                    , text " | "
                    , cardUi k
                    ]
                ]

        StraightFlush c1 c2 c3 c4 c5 ->
            column
                []
                [ text "Straight flush"
                , row
                    []
                    [ cardUi c1
                    , cardUi c2
                    , cardUi c3
                    , cardUi c4
                    , cardUi c5
                    ]
                ]


communityCardsUi : Round -> Element Msg
communityCardsUi round =
    row
        [ width fill ]
    <|
        case round of
            PreFlopRound ->
                []

            FlopRound flop1 flop2 flop3 ->
                List.map cardUi [ flop1, flop2, flop3 ]

            TurnRound flop1 flop2 flop3 turn ->
                List.map cardUi [ flop1, flop2, flop3, turn ]

            RiverRound flop1 flop2 flop3 turn river ->
                List.map cardUi [ flop1, flop2, flop3, turn, river ]

            ShowdownRound flop1 flop2 flop3 turn river _ ->
                List.map cardUi [ flop1, flop2, flop3, turn, river ]



-- view for inspecting the design elements


uiElements : Int -> Element Msg
uiElements seed =
    let
        handsGen =
            Random.andThen
                (\record ->
                    Random.map4
                        (\fh foak sf rf ->
                            { highCard = record.highCard
                            , pair = record.pair
                            , twoPair = record.twoPair
                            , threeOfAKind = record.threeOfAKind
                            , straight = record.straight
                            , flush = record.flush
                            , fullHouse = fh
                            , fourOfAKind = foak
                            , straightFlush = sf
                            , royalFlush = sf
                            }
                        )
                        fullHouseHandGen
                        fourOfAKindHandGen
                        straightFlushHandGen
                        royalFlushHandGen
                )
            <|
                Random.Extra.map6
                    (\hc p tp toak s f ->
                        { highCard = hc
                        , pair = p
                        , twoPair = tp
                        , threeOfAKind = toak
                        , straight = s
                        , flush = f
                        }
                    )
                    highCardHandGen
                    pairHandGen
                    twoPairHandGen
                    threeOfAKindHandGen
                    straightHandGen
                    flushHandGen

        ( { hands, flopRound, turnRound, riverRound }, seed2 ) =
            Random.step
                (Random.map4
                    (\h f t r ->
                        { hands = h
                        , flopRound = f
                        , turnRound = t
                        , riverRound = r
                        }
                    )
                    handsGen
                    flopRoundGen
                    turnRoundGen
                    riverRoundGen
                )
            <|
                Random.initialSeed seed
    in
    column
        [ spacing 15 ]
        [ handUi hands.highCard
        , handUi hands.pair
        , handUi hands.twoPair
        , handUi hands.threeOfAKind
        , handUi hands.straight
        , handUi hands.flush
        , handUi hands.fullHouse
        , handUi hands.fourOfAKind
        , handUi hands.straight
        , handUi hands.royalFlush
        , communityCardsUi flopRound
        , communityCardsUi turnRound
        , communityCardsUi riverRound
        ]
