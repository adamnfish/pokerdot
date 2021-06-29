module Views.Elements exposing (cardUi, communityCardsUi, dotContainer, handUi, hiddenCardUi, pdButton, pdButtonSmall, pdTab, pdText, selfUi, tableUi, uiElements, zWidths)

import Browser.Dom exposing (Viewport)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (labelAbove, labelLeft)
import Element.Region as Region
import FontAwesome.Attributes
import FontAwesome.Icon
import FontAwesome.Regular
import FontAwesome.Solid
import Model exposing (Card, Game, Hand(..), Msg(..), Player, Rank(..), Round(..), Self)
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
                        [ spacing 2 ]
                    <|
                        if isPeeking then
                            [ cardUi 0 card1, cardUi 0 card2 ]

                        else
                            [ hiddenCardUi, hiddenCardUi ]
            , Input.button
                []
                { onPress = Just TogglePeek
                , label =
                    if isPeeking then
                        Element.html <|
                            (FontAwesome.Regular.eyeSlash
                                |> FontAwesome.Icon.present
                                |> FontAwesome.Icon.styled [ FontAwesome.Attributes.fa2x ]
                                |> FontAwesome.Icon.withId "self-peeking-toggle_pokerdot"
                                |> FontAwesome.Icon.titled "Stop looking at hand"
                                |> FontAwesome.Icon.view
                            )

                    else
                        Element.html <|
                            (FontAwesome.Regular.eye
                                |> FontAwesome.Icon.present
                                |> FontAwesome.Icon.styled [ FontAwesome.Attributes.fa2x ]
                                |> FontAwesome.Icon.withId "self-peeking-toggle_pokerdot"
                                |> FontAwesome.Icon.titled "Look at hand"
                                |> FontAwesome.Icon.view
                            )
                }
            ]


hiddenCardUi : Element Msg
hiddenCardUi =
    let
        bgColour =
            rgb255 253 253 253

        textColour =
            rgb255 40 40 40
    in
    Element.el
        [ height <| px 76
        , width <| px 76
        , Border.rounded 38
        , Background.color bgColour
        , Border.width 2
        , Border.color textColour
        , clip
        , Font.size 25
        , Font.color textColour
        ]
    <|
        row
            [ centerX
            , centerY
            ]
            [ text "x" ]


cardUi : Int -> Card -> Element Msg
cardUi offsetIndex card =
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

        ( suit, textColour, bgColour ) =
            case card.suit of
                Model.Clubs ->
                    ( text "♣"
                    , rgb255 40 40 40
                    , rgb255 253 255 255
                    )

                Model.Diamonds ->
                    ( text "♦"
                    , rgb255 150 20 20
                    , rgb255 255 253 253
                    )

                Model.Spades ->
                    ( text "♠"
                    , rgb255 40 40 40
                    , rgb255 253 255 253
                    )

                Model.Hearts ->
                    ( text "♥"
                    , rgb255 150 20 20
                    , rgb255 255 253 253
                    )
    in
    Element.el
        [ height <| px 76
        , width <| px 76
        , Border.rounded 38
        , Background.color bgColour
        , Border.width 2
        , Border.color textColour
        , clip
        , Font.size 25
        , Font.color textColour
        , moveLeft <| toFloat offsetIndex * 10

        --, Background.color <| rgb255 200 200 200
        ]
    <|
        row
            [ centerX
            , centerY
            ]
            [ rank, suit ]


handUi : String -> Int -> Hand -> Element Msg
handUi name winnings hand =
    let
        cardSpacing =
            2

        ( label, cardEls ) =
            case hand of
                HighCard c1 c2 c3 c4 c5 ->
                    ( "HIGH CARD"
                    , [ cardUi 0 c1
                      , cardUi 0 c2
                      , cardUi 0 c3
                      , cardUi 0 c4
                      , cardUi 0 c5
                      ]
                    )

                Pair p1 p2 k1 k2 k3 ->
                    ( "PAIR"
                    , [ cardUi 0 p1
                      , cardUi 1 p2
                      , cardUi 0 k1
                      , cardUi 0 k2
                      , cardUi 0 k3
                      ]
                    )

                TwoPair p11 p12 p21 p22 k ->
                    ( "TWO PAIR"
                    , [ cardUi 0 p11
                      , cardUi 1 p12
                      , cardUi 0 p21
                      , cardUi 1 p22
                      , cardUi 0 k
                      ]
                    )

                ThreeOfAKind t1 t2 t3 k1 k2 ->
                    ( "THREE OF A KIND"
                    , [ cardUi 0 t1
                      , cardUi 1 t2
                      , cardUi 2 t3
                      , cardUi 0 k1
                      , cardUi 0 k2
                      ]
                    )

                Straight c1 c2 c3 c4 c5 ->
                    ( "STRAIGHT"
                    , [ cardUi 0 c1
                      , cardUi 1 c2
                      , cardUi 2 c3
                      , cardUi 3 c4
                      , cardUi 4 c5
                      ]
                    )

                Flush c1 c2 c3 c4 c5 ->
                    ( "FLUSH"
                    , [ cardUi 0 c1
                      , cardUi 1 c2
                      , cardUi 2 c3
                      , cardUi 3 c4
                      , cardUi 4 c5
                      ]
                    )

                FullHouse t1 t2 t3 p1 p2 ->
                    ( "FULL HOUSE"
                    , [ cardUi 0 t1
                      , cardUi 1 t2
                      , cardUi 2 t3
                      , cardUi 0 p1
                      , cardUi 1 p2
                      ]
                    )

                FourOfAKind q1 q2 q3 q4 k ->
                    ( "FOUR OF A KIND"
                    , [ cardUi 0 q1
                      , cardUi 1 q2
                      , cardUi 2 q3
                      , cardUi 3 q4
                      , cardUi 0 k
                      ]
                    )

                StraightFlush c1 c2 c3 c4 c5 ->
                    ( if c1.rank == Ten then
                        "ROYAL FLUSH"

                      else
                        "STRAIGHT FLUSH"
                    , [ cardUi 0 c1
                      , cardUi 1 c2
                      , cardUi 2 c3
                      , cardUi 3 c4
                      , cardUi 4 c5
                      ]
                    )

        winningsIcon =
            if winnings > 0 then
                FontAwesome.Solid.angleDoubleUp

            else
                FontAwesome.Solid.angleDown
    in
    column
        [ width fill
        , spacing 5
        , padding 10
        , Background.color <| rgb255 50 50 50
        ]
        [ el
            [ Font.size 25
            , Font.color <| rgba255 250 250 250 0.8
            , Font.shadow
                { offset = ( 1, 1 )
                , blur = 0.5
                , color = rgba255 100 100 100 0.8
                }
            , paddingEach
                { top = 0
                , bottom = 2
                , left = 5
                , right = 10
                }
            ]
          <|
            text name

        -- TODO: Include player hole display here
        , row
            [ spacing cardSpacing ]
          <|
            List.append cardEls
                [ el
                    [ Font.size 25
                    , Font.color <| rgba255 250 250 250 0.8
                    , Font.shadow
                        { offset = ( 1, 1 )
                        , blur = 0.5
                        , color = rgba255 100 100 100 0.8
                        }
                    , paddingEach
                        { top = 0
                        , bottom = 2
                        , left = 10
                        , right = 10
                        }
                    ]
                  <|
                    row
                        []
                        [ Element.html <|
                            (winningsIcon
                                |> FontAwesome.Icon.present
                                |> FontAwesome.Icon.styled [ FontAwesome.Attributes.sm ]
                                |> FontAwesome.Icon.withId ("self-peeking-toggle_pokerdot_" ++ name)
                                |> FontAwesome.Icon.titled "Winnings"
                                |> FontAwesome.Icon.view
                            )
                        , text " "
                        , text <| String.fromInt winnings
                        ]
                ]
        , el
            [ Font.size 15
            , Font.color <| rgba255 250 250 250 0.8
            , Font.shadow
                { offset = ( 1, 1 )
                , blur = 0.5
                , color = rgba255 100 100 100 0.8
                }
            , paddingEach
                { top = 0
                , bottom = 2
                , left = 5
                , right = 10
                }
            ]
          <|
            text label
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
                [ cardUi 0 flop1
                , cardUi 0 flop2
                , cardUi 0 flop3
                ]

            TurnRound flop1 flop2 flop3 turn ->
                [ cardUi 0 flop1
                , cardUi 0 flop2
                , cardUi 0 flop3
                , cardUi 0 turn
                ]

            RiverRound flop1 flop2 flop3 turn river ->
                [ cardUi 0 flop1
                , cardUi 0 flop2
                , cardUi 0 flop3
                , cardUi 0 turn
                , cardUi 0 river
                ]

            ShowdownRound flop1 flop2 flop3 turn river _ ->
                [ cardUi 0 flop1
                , cardUi 0 flop2
                , cardUi 0 flop3
                , cardUi 0 turn
                , cardUi 0 river
                ]



-- view for inspecting the design elements


uiElements : Int -> Element Msg
uiElements seed =
    let
        namesGen =
            Random.list 11 nameGen

        winningsGen =
            Random.list 11 <| Random.int 0 150

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
                            , royalFlush = rf
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

        ( { hands, flopRound, turnRound, riverRound, names, winnings }, seed2 ) =
            Random.step
                (Random.Extra.map6
                    (\h f t r n w ->
                        { hands = h
                        , flopRound = f
                        , turnRound = t
                        , riverRound = r
                        , names = n
                        , winnings = w
                        }
                    )
                    handsGen
                    flopRoundGen
                    turnRoundGen
                    riverRoundGen
                    namesGen
                    winningsGen
                )
            <|
                Random.initialSeed seed

        getName i =
            Maybe.withDefault
                "abc"
            <|
                List.head <|
                    List.drop i names

        getWinnings i =
            Maybe.withDefault
                -1
            <|
                List.head <|
                    List.drop i winnings
    in
    column
        [ width fill
        , spacing 5
        ]
        [ row
            [ width fill
            , spacing 25
            ]
            [ Input.button
                [ height <| px 25
                , width <| px 25
                ]
                { onPress = Just (NavigateUIElements <| seed - 1)
                , label = text "-"
                }
            , Input.slider
                [ height <| px 30
                , width <| px 250

                -- Here is where we're creating/styling the "track"
                , Element.behindContent
                    (Element.el
                        [ width fill
                        , height (px 2)
                        , centerY
                        , Background.color <| rgb255 0 0 0
                        , Border.rounded 2
                        ]
                        Element.none
                    )
                ]
                { onChange = round >> NavigateUIElements
                , label =
                    Input.labelAbove []
                        (text "Seed")
                , min = 0
                , max = 400
                , step = Nothing
                , value = toFloat seed
                , thumb =
                    Input.defaultThumb
                }
            , Input.button
                [ height <| px 25
                , width <| px 25
                ]
                { onPress = Just (NavigateUIElements <| seed + 1)
                , label = text "+"
                }
            ]
        , handUi (getName 0) (getWinnings 0) hands.highCard
        , handUi (getName 1) (getWinnings 1) hands.pair
        , handUi (getName 2) (getWinnings 2) hands.twoPair
        , handUi (getName 3) (getWinnings 3) hands.threeOfAKind
        , handUi (getName 4) (getWinnings 4) hands.straight
        , handUi (getName 5) (getWinnings 5) hands.flush
        , handUi (getName 6) (getWinnings 6) hands.fullHouse
        , handUi (getName 7) (getWinnings 7) hands.fourOfAKind
        , handUi (getName 8) (getWinnings 8) hands.straight
        , handUi (getName 9) (getWinnings 9) hands.straightFlush
        , handUi (getName 10) (getWinnings 10) hands.royalFlush
        , communityCardsUi flopRound
        , communityCardsUi turnRound
        , communityCardsUi riverRound
        ]
