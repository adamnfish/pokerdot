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
import List.Extra
import Maybe.Extra
import Model exposing (Card, Game, Hand(..), Msg(..), Player, PlayerId(..), Rank(..), Round(..), Self)
import Random
import Random.Extra
import Views.Generators exposing (..)


type CardSize
    = SmallCard
    | NormalCard


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


tableUi : Round -> List Player -> Element Msg
tableUi round players =
    let
        pot =
            List.sum <| List.map .pot players

        seat : Player -> Element Msg
        seat player =
            let
                betIcon =
                    if player.bet > 0 then
                        FontAwesome.Solid.caretRight

                    else
                        FontAwesome.Regular.circle
            in
            row
                [ width fill
                , spacing 8
                , padding 8
                , Background.color <| rgb255 200 200 200
                ]
                [ el
                    [ width fill
                    , Font.alignLeft
                    , clip
                    ]
                  <|
                    text player.screenName
                , el
                    [ width <| px 50
                    , Font.alignRight
                    ]
                  <|
                    text <|
                        String.fromInt player.stack
                , row
                    [ width <| px 60 ]
                    [ html
                        (betIcon
                            |> FontAwesome.Icon.present
                            |> FontAwesome.Icon.styled [ FontAwesome.Attributes.sm ]
                            |> FontAwesome.Icon.withId "self-peeking-toggle_pokerdot"
                            |> FontAwesome.Icon.titled "Stop looking at hand"
                            |> FontAwesome.Icon.view
                        )
                    , text " "
                    , if player.bet > 0 then
                        text <|
                            String.fromInt player.bet

                      else
                        Element.none
                    ]
                ]
    in
    column
        [ width fill
        ]
        [ column
            [ width fill
            , spacing 2
            ]
          <|
            List.map seat players
        , case round of
            PreFlopRound ->
                Element.none

            _ ->
                row
                    [ width fill
                    , paddingXY 8 2
                    ]
                    [ el
                        [ width fill
                        , clip
                        ]
                      <|
                        communityCardsUi round
                    , column
                        [ paddingXY 15 8
                        , spacing 5
                        , Border.width 2
                        , Border.color <| rgb255 50 50 50
                        ]
                        [ el
                            [ width fill
                            , Font.size 15
                            ]
                          <|
                            text "pot"
                        , el
                            [ Font.size 22
                            ]
                          <|
                            text <|
                                String.fromInt pot
                        ]
                    ]
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
                            [ cardUi 0 NormalCard card1, cardUi 0 NormalCard card2 ]

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


cardUi : Int -> CardSize -> Card -> Element Msg
cardUi offsetIndex cardSize card =
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

        ( radius, fontSize, offset ) =
            case cardSize of
                SmallCard ->
                    ( 40, 14, 5 )

                NormalCard ->
                    ( 56, 19, 10 )
    in
    Element.el
        [ height <| px radius
        , width <| px radius
        , Border.rounded (radius // 2)
        , Background.color bgColour
        , Border.width <|
            case cardSize of
                SmallCard ->
                    1

                NormalCard ->
                    2
        , Border.color textColour
        , clip
        , Font.size fontSize
        , Font.color textColour
        , moveLeft <| toFloat offsetIndex * offset

        --, Background.color <| rgb255 200 200 200
        ]
    <|
        row
            [ centerX
            , centerY
            ]
            [ rank, suit ]


handUi : String -> Int -> Maybe ( Card, Card ) -> Hand -> Element Msg
handUi name winnings maybeHole hand =
    let
        cardSpacing =
            1

        ( label, cardEls ) =
            case hand of
                HighCard c1 c2 c3 c4 c5 ->
                    ( "HIGH CARD"
                    , [ cardUi 0 NormalCard c1
                      , cardUi 0 NormalCard c2
                      , cardUi 0 NormalCard c3
                      , cardUi 0 NormalCard c4
                      , cardUi 0 NormalCard c5
                      ]
                    )

                Pair p1 p2 k1 k2 k3 ->
                    ( "PAIR"
                    , [ cardUi 0 NormalCard p1
                      , cardUi 1 NormalCard p2
                      , cardUi 0 NormalCard k1
                      , cardUi 0 NormalCard k2
                      , cardUi 0 NormalCard k3
                      ]
                    )

                TwoPair p11 p12 p21 p22 k ->
                    ( "TWO PAIR"
                    , [ cardUi 0 NormalCard p11
                      , cardUi 1 NormalCard p12
                      , cardUi 0 NormalCard p21
                      , cardUi 1 NormalCard p22
                      , cardUi 0 NormalCard k
                      ]
                    )

                ThreeOfAKind t1 t2 t3 k1 k2 ->
                    ( "THREE OF A KIND"
                    , [ cardUi 0 NormalCard t1
                      , cardUi 1 NormalCard t2
                      , cardUi 2 NormalCard t3
                      , cardUi 0 NormalCard k1
                      , cardUi 0 NormalCard k2
                      ]
                    )

                Straight c1 c2 c3 c4 c5 ->
                    ( "STRAIGHT"
                    , [ cardUi 0 NormalCard c1
                      , cardUi 1 NormalCard c2
                      , cardUi 2 NormalCard c3
                      , cardUi 3 NormalCard c4
                      , cardUi 4 NormalCard c5
                      ]
                    )

                Flush c1 c2 c3 c4 c5 ->
                    ( "FLUSH"
                    , [ cardUi 0 NormalCard c1
                      , cardUi 1 NormalCard c2
                      , cardUi 2 NormalCard c3
                      , cardUi 3 NormalCard c4
                      , cardUi 4 NormalCard c5
                      ]
                    )

                FullHouse t1 t2 t3 p1 p2 ->
                    ( "FULL HOUSE"
                    , [ cardUi 0 NormalCard t1
                      , cardUi 1 NormalCard t2
                      , cardUi 2 NormalCard t3
                      , cardUi 0 NormalCard p1
                      , cardUi 1 NormalCard p2
                      ]
                    )

                FourOfAKind q1 q2 q3 q4 k ->
                    ( "FOUR OF A KIND"
                    , [ cardUi 0 NormalCard q1
                      , cardUi 1 NormalCard q2
                      , cardUi 2 NormalCard q3
                      , cardUi 3 NormalCard q4
                      , cardUi 0 NormalCard k
                      ]
                    )

                StraightFlush c1 c2 c3 c4 c5 ->
                    ( if c1.rank == Ten then
                        "ROYAL FLUSH"

                      else
                        "STRAIGHT FLUSH"
                    , [ cardUi 0 NormalCard c1
                      , cardUi 1 NormalCard c2
                      , cardUi 2 NormalCard c3
                      , cardUi 3 NormalCard c4
                      , cardUi 4 NormalCard c5
                      ]
                    )

        winningsIcon =
            if winnings > 0 then
                FontAwesome.Solid.sortUp

            else
                FontAwesome.Solid.sortDown
    in
    column
        [ width fill
        , spacing 5
        , paddingXY 8 10
        , Background.color <| rgb255 50 50 50
        ]
        [ row
            [ width fill
            , height <| px 40
            ]
            [ case maybeHole of
                Just ( card1, card2 ) ->
                    row
                        []
                        [ cardUi 0 SmallCard card1
                        , cardUi 1 SmallCard card2
                        ]

                Nothing ->
                    Element.none
            , el
                [ width fill
                , clip
                , Font.size 20
                , Font.alignLeft
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
            ]

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
        [ width fill
        , spacing 2
        ]
    <|
        case round of
            PreFlopRound ->
                []

            FlopRound flop1 flop2 flop3 ->
                [ cardUi 0 NormalCard flop1
                , cardUi 1 NormalCard flop2
                , cardUi 2 NormalCard flop3
                ]

            TurnRound flop1 flop2 flop3 turn ->
                [ cardUi 0 NormalCard flop1
                , cardUi 1 NormalCard flop2
                , cardUi 2 NormalCard flop3
                , cardUi 3 NormalCard turn
                ]

            RiverRound flop1 flop2 flop3 turn river ->
                [ cardUi 0 NormalCard flop1
                , cardUi 1 NormalCard flop2
                , cardUi 2 NormalCard flop3
                , cardUi 3 NormalCard turn
                , cardUi 4 NormalCard river
                ]

            ShowdownRound flop1 flop2 flop3 turn river _ ->
                [ cardUi 0 NormalCard flop1
                , cardUi 1 NormalCard flop2
                , cardUi 2 NormalCard flop3
                , cardUi 3 NormalCard turn
                , cardUi 4 NormalCard river
                ]



-- view for inspecting the design elements


uiElements : Int -> Element Msg
uiElements seed =
    let
        handsCount =
            11

        namesGen =
            Random.list handsCount nameGen

        winningGen =
            Random.andThen
                (\b ->
                    if b then
                        Random.int 0 150

                    else
                        Random.constant 0
                )
            <|
                Random.weighted
                    ( 5.0, True )
                    [ ( 1.0, False ) ]

        winningsGen =
            Random.list handsCount winningGen

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

        holesGen =
            Random.list handsCount (Random.Extra.maybe Random.Extra.bool <| holeGen)

        playerGen =
            Random.map4
                (\name stack pot bet ->
                    { playerId = Pid "player-id"
                    , screenName = name
                    , isAdmin = False
                    , isHost = False
                    , stack = stack
                    , pot = pot
                    , bet = bet
                    , folded = False
                    , busted = False
                    , hole = Nothing
                    }
                )
                nameGen
                (Random.int 0 1000)
                winningGen
                winningGen

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

        ( { holes, players, round }, seed3 ) =
            Random.step
                (Random.map3
                    (\h ps r ->
                        { holes = h
                        , players = ps
                        , round = r
                        }
                    )
                    holesGen
                    (Random.list 5 playerGen)
                    roundGen
                )
                seed2

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

        getHole i =
            Maybe.Extra.join <| List.head <| List.drop i holes

        getPlayer i =
            Maybe.withDefault
                { playerId = Pid "player-id"
                , screenName = "couldn't lookup random player"
                , isAdmin = False
                , isHost = False
                , stack = 0
                , pot = 0
                , bet = 0
                , folded = False
                , busted = False
                , hole = Nothing
                }
            <|
                List.head <|
                    List.drop i players
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
                , width fill

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
                { onChange = Basics.round >> NavigateUIElements
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
        , tableUi round
            [ getPlayer 0
            , getPlayer 1
            , getPlayer 2
            , getPlayer 3
            , getPlayer 4
            ]
        , handUi (getName 0) (getWinnings 0) (getHole 0) hands.highCard
        , handUi (getName 1) (getWinnings 1) (getHole 1) hands.pair
        , handUi (getName 2) (getWinnings 2) (getHole 2) hands.twoPair
        , handUi (getName 3) (getWinnings 3) (getHole 3) hands.threeOfAKind
        , handUi (getName 4) (getWinnings 4) (getHole 4) hands.straight
        , handUi (getName 5) (getWinnings 5) (getHole 5) hands.flush
        , handUi (getName 6) (getWinnings 6) (getHole 6) hands.fullHouse
        , handUi (getName 7) (getWinnings 7) (getHole 7) hands.fourOfAKind
        , handUi (getName 8) (getWinnings 8) (getHole 8) hands.straight
        , handUi (getName 9) (getWinnings 9) (getHole 9) hands.straightFlush
        , handUi (getName 10) (getWinnings 10) (getHole 10) hands.royalFlush
        , communityCardsUi flopRound
        , communityCardsUi turnRound
        , communityCardsUi riverRound
        ]
