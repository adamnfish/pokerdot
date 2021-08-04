module Views.Elements exposing (cardUi, communityCardsUi, connectionUi, container, controlsButton, handUi, hiddenCardUi, pdTab, pdText, pokerControlsUi, selfUi, tableUi, uiElements, zWidths)

import Browser.Dom exposing (Viewport)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import FontAwesome.Attributes
import FontAwesome.Icon
import FontAwesome.Regular
import FontAwesome.Solid
import Html.Attributes
import Maybe.Extra
import Model exposing (ActSelection(..), Card, Game, Hand(..), Model, Msg(..), Player, PlayerId(..), Rank(..), Round(..), Self)
import Random
import Random.Extra
import Views.Generators exposing (..)
import Views.Theme as Theme


type CardSize
    = SmallCard
    | NormalCard
    | LargeCard


container : Viewport -> Element msg -> Element msg
container viewport content =
    el
        [ width <| maximum (round viewport.viewport.width - 24) <| px 500
        , centerX
        ]
        content


controlsButton : Theme.Scheme -> Msg -> Element Msg -> Element Msg
controlsButton scheme msg label =
    Input.button
        [ width <| minimum 100 shrink
        , height <| px 90
        , Border.rounded 2
        , Border.solid
        , Border.width 2
        , Border.color Theme.colours.black
        , Border.shadow
            { offset = ( 5, 5 )
            , size = 0
            , blur = 0
            , color = Theme.glow scheme.highlight
            }
        , Font.size 25
        , Font.color <| Theme.textColour scheme.text
        , Background.color scheme.highlight
        , focused
            [ Background.color <| Theme.focusColour scheme.highlight
            , Border.color Theme.colours.white
            , Border.shadow
                { offset = ( 5, 5 )
                , size = 0
                , blur = 0
                , color = Theme.glow <| Theme.focusColour scheme.highlight
                }
            ]
        ]
        { onPress = Just msg
        , label = label
        }


pdTab : Bool -> Msg -> String -> Element Msg
pdTab active msg label =
    let
        a =
            1
    in
    Input.button
        [ paddingXY 8 5
        , Border.rounded 1
        , Border.solid
        , Border.width 2
        , Border.color <|
            if active then
                Theme.colours.lowlight

            else
                Theme.colours.black
        , if active then
            Border.shadow
                { offset = ( 2, 2 )
                , size = 0
                , blur = 0
                , color = Theme.dim Theme.colours.highlightSecondary
                }

          else
            Border.shadow
                { offset = ( 5, 5 )
                , size = 0
                , blur = 0
                , color = Theme.glow Theme.colours.highlightSecondary
                }
        , Background.color <|
            if active then
                Theme.colours.highlightSecondary

            else
                Theme.colours.highlightPrimary
        , Font.color <| Theme.textColour Theme.colours.black
        , focused
            [ Background.color <| Theme.focusColour Theme.colours.highlightSecondary
            , Border.color Theme.colours.white
            , Font.color <| Theme.textColour Theme.colours.white
            ]
        ]
        { onPress = Just msg
        , label = text label
        }


pdText : (String -> msg) -> String -> String -> Element msg
pdText msg value labelStr =
    Input.text
        [ Font.alignLeft
        , paddingXY 10 8
        , Border.solid
        , Border.width 2
        , Border.color Theme.colours.black
        , Border.widthEach { zWidths | bottom = 2 }
        , Border.rounded 0
        , Background.color Theme.colours.white
        , focused
            [ Background.color Theme.colours.highlightPrimary
            , Border.color Theme.colours.white
            ]
        ]
        { onChange = msg
        , text = value
        , placeholder = Nothing
        , label =
            Input.labelAbove
                [ alignLeft ]
            <|
                el
                    [ Font.color Theme.colours.white ]
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


connectionUi : Bool -> Element Msg
connectionUi connected =
    if connected then
        html <|
            (FontAwesome.Solid.link
                |> FontAwesome.Icon.present
                |> FontAwesome.Icon.styled [ FontAwesome.Attributes.sm ]
                |> FontAwesome.Icon.withId "pokerdot-header-connected_pokerdot"
                |> FontAwesome.Icon.titled "Connected"
                |> FontAwesome.Icon.view
            )

    else
        html <|
            (FontAwesome.Solid.unlink
                |> FontAwesome.Icon.present
                |> FontAwesome.Icon.styled [ FontAwesome.Attributes.sm ]
                |> FontAwesome.Icon.withId "pokerdot-header-disconnected_pokerdot"
                |> FontAwesome.Icon.titled "Not connected"
                |> FontAwesome.Icon.view
            )


tableUi : Round -> List Player -> Element Msg
tableUi round players =
    let
        pot =
            List.sum <| List.map .pot players

        scheme =
            Theme.scheme1

        seat : Player -> Element Msg
        seat player =
            row
                [ width fill
                , spacing 8
                , padding 8
                , Background.color scheme.main
                ]
                [ el
                    [ width fill
                    , Font.alignLeft
                    , Font.color <| Theme.textColour Theme.colours.black
                    , clip
                    ]
                  <|
                    text player.screenName
                , el
                    [ width <| px 50
                    , Font.alignRight
                    , Font.color <| Theme.textColour Theme.colours.black
                    ]
                  <|
                    text <|
                        String.fromInt player.stack
                , row
                    [ width <| px 60
                    , Font.color <| Theme.textColour Theme.colours.black
                    ]
                    [ if player.bet > 0 then
                        html <|
                            (FontAwesome.Solid.caretRight
                                |> FontAwesome.Icon.present
                                |> FontAwesome.Icon.styled [ FontAwesome.Attributes.sm ]
                                |> FontAwesome.Icon.withId ("table-ui-bet-display_pokerdot_" ++ player.screenName)
                                |> FontAwesome.Icon.view
                            )

                      else
                        text "0"
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
        , padding 8
        , spacing 8
        , Background.color Theme.colours.lowlight
        ]
        [ column
            [ width fill
            , spacing 2
            ]
          <|
            List.map seat players
        , row
            [ width fill
            , paddingXY 0 2
            ]
            [ el
                [ width fill
                , clip
                ]
              <|
                communityCardsUi round
            , column
                [ width (minimum 80 shrink)
                , paddingXY 15 8
                , spacing 3
                , Background.color Theme.colours.primary
                ]
                [ el
                    [ width fill
                    , paddingEach
                        { top = 0
                        , bottom = 2
                        , left = 0
                        , right = 0
                        }
                    , Font.size 15
                    , Font.center
                    ]
                  <|
                    text "pot"
                , el
                    [ width fill
                    , Font.size 22
                    , Font.center
                    ]
                  <|
                    text <|
                        if pot == 0 then
                            "-"

                        else
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
            [ case self.hole of
                Nothing ->
                    text " - "

                Just ( card1, card2 ) ->
                    row
                        [ spacing 2 ]
                    <|
                        if isPeeking then
                            [ cardUi 0 LargeCard card1, cardUi 0 LargeCard card2 ]

                        else
                            [ hiddenCardUi, hiddenCardUi ]
            , el
                [ alignRight ]
              <|
                controlsButton Theme.scheme3 TogglePeek <|
                    column
                        [ spacing 5
                        , centerX
                        , centerY
                        , Font.center
                        ]
                        [ text "peek"
                        , if isPeeking then
                            el
                                [ alignRight
                                , centerX
                                ]
                            <|
                                Element.html <|
                                    (FontAwesome.Regular.eyeSlash
                                        |> FontAwesome.Icon.present
                                        |> FontAwesome.Icon.styled [ FontAwesome.Attributes.sm ]
                                        |> FontAwesome.Icon.withId "self-peeking-toggle_pokerdot"
                                        |> FontAwesome.Icon.titled "Stop looking at hand"
                                        |> FontAwesome.Icon.view
                                    )

                          else
                            el
                                [ alignRight
                                , centerX
                                ]
                            <|
                                Element.html <|
                                    (FontAwesome.Regular.eye
                                        |> FontAwesome.Icon.present
                                        |> FontAwesome.Icon.styled [ FontAwesome.Attributes.sm ]
                                        |> FontAwesome.Icon.withId "self-peeking-toggle_pokerdot"
                                        |> FontAwesome.Icon.titled "Look at hand"
                                        |> FontAwesome.Icon.view
                                    )
                        ]
            ]


pokerControlsUi : Bool -> Int -> ActSelection -> Self -> List Player -> Element Msg
pokerControlsUi isActive smallBlind actSelection self players =
    let
        playerBets =
            List.map .bet players

        highestBet =
            Maybe.withDefault 0 <| List.maximum playerBets

        -- TODO: this is not yet correct
        --       make sure it takes into account all-in calls and the minimum bet amount
        callAmount =
            highestBet - self.bet

        -- TODO: this is not yet correct
        --       the minimum raise might instead be the size of the most recent raise
        minimumRaise =
            callAmount + (2 * smallBlind)

        currentSelectedBetAmount =
            case actSelection of
                ActBet amount ->
                    amount

                _ ->
                    minimumRaise

        buttonHiddenAttrs hidden =
            if hidden then
                [ transparent True
                , htmlAttribute <| Html.Attributes.style "visibility" "hidden"
                ]

            else
                []
    in
    column
        [ width fill
        , spacing 20
        , behindContent <|
            el
                [ width <| px 260
                , height <| px 260
                , centerX
                , centerY
                , Background.color Theme.scheme1.main
                , Border.rounded 130
                , clip
                ]
            <|
                if isActive then
                    Element.none

                else
                    el
                        [ width fill
                        , alignBottom
                        , paddingXY 0 50
                        , Font.color <| Theme.textColour Theme.colours.black
                        , Font.size 18
                        ]
                    <|
                        text "waiting"
        ]
        [ el
            [ centerX ]
          <|
            -- or all-in, when betting
            case actSelection of
                ActBet _ ->
                    el (buttonHiddenAttrs <| not isActive) <|
                        controlsButton Theme.scheme1 (Bet self.stack) <|
                            column
                                [ width fill
                                , spacing 5
                                ]
                                [ el
                                    [ width fill
                                    , Font.center
                                    ]
                                  <|
                                    text "all-in"
                                , el
                                    [ width fill
                                    , Font.center
                                    , Font.size 18
                                    ]
                                  <|
                                    text (String.fromInt self.stack)
                                ]

                _ ->
                    el (buttonHiddenAttrs <| not isActive) <|
                        controlsButton Theme.scheme1 Fold <|
                            text "fold"
        , row
            [ centerX
            ]
            [ el
                (case actSelection of
                    ActBet betAmount ->
                        [ inFront
                            (el
                                (List.append
                                    [ width fill
                                    , moveUp 100
                                    ]
                                    (buttonHiddenAttrs <| not isActive)
                                )
                             <|
                                Input.slider
                                    [ width <| px 50
                                    , height <| px 250
                                    , Element.behindContent
                                        (Element.el
                                            [ width <| px 20
                                            , height fill
                                            , centerX
                                            , Background.color Theme.colours.highlightSecondary
                                            , Border.rounded 2
                                            , Border.solid
                                            , Border.width 2
                                            , Border.color Theme.colours.black
                                            ]
                                            Element.none
                                        )
                                    ]
                                    { onChange = Basics.round >> InputBet
                                    , label =
                                        Input.labelAbove
                                            [ width fill ]
                                        <|
                                            row
                                                [ width fill
                                                , centerX
                                                ]
                                                [ Input.button
                                                    [ height <| px 27
                                                    , width <| px 27
                                                    , Font.color <| Theme.textColour Theme.colours.black
                                                    , Background.color Theme.scheme1.highlight
                                                    , Border.rounded 5
                                                    , Border.width 2
                                                    , Border.color Theme.colours.black
                                                    ]
                                                    { onPress = Just (InputBet <| betAmount - 1)
                                                    , label =
                                                        el
                                                            [ centerX, centerY ]
                                                        <|
                                                            html
                                                                (FontAwesome.Solid.minus
                                                                    |> FontAwesome.Icon.present
                                                                    |> FontAwesome.Icon.styled [ FontAwesome.Attributes.sm ]
                                                                    |> FontAwesome.Icon.view
                                                                )
                                                    }
                                                , el
                                                    [ width <| px 50 ]
                                                  <|
                                                    text <|
                                                        String.fromInt betAmount
                                                , Input.button
                                                    [ height <| px 27
                                                    , width <| px 27
                                                    , Font.color <| Theme.textColour Theme.colours.black
                                                    , Background.color Theme.scheme1.highlight
                                                    , Border.rounded 2
                                                    , Border.width 2
                                                    , Border.color Theme.colours.black
                                                    ]
                                                    { onPress = Just (InputBet <| betAmount + 1)
                                                    , label =
                                                        el
                                                            [ centerX, centerY ]
                                                        <|
                                                            html
                                                                (FontAwesome.Solid.plus
                                                                    |> FontAwesome.Icon.present
                                                                    |> FontAwesome.Icon.styled [ FontAwesome.Attributes.sm ]
                                                                    |> FontAwesome.Icon.view
                                                                )
                                                    }
                                                ]
                                    , min = toFloat minimumRaise
                                    , max = toFloat self.stack
                                    , step = Just <| toFloat (smallBlind * 2)
                                    , value = toFloat <| max betAmount minimumRaise
                                    , thumb =
                                        Input.thumb
                                            [ width <| px 50
                                            , height <| px 50
                                            , Background.color Theme.scheme1.highlight
                                            , focused
                                                [ Background.color <| Theme.focusColour Theme.scheme1.highlight
                                                , Border.color Theme.colours.white
                                                , Border.shadow
                                                    { offset = ( 3, 3 )
                                                    , size = 0
                                                    , blur = 0
                                                    , color = Theme.glow Theme.scheme1.highlight
                                                    }
                                                ]
                                            , Border.solid
                                            , Border.rounded 2
                                            , Border.width 2
                                            , Border.color Theme.colours.black
                                            , Border.shadow
                                                { offset = ( 3, 3 )
                                                , size = 0
                                                , blur = 0
                                                , color = Theme.glow Theme.scheme1.highlight
                                                }
                                            , inFront <|
                                                column
                                                    [ moveRight 15
                                                    , moveDown 6
                                                    , Font.color Theme.colours.white
                                                    ]
                                                    [ html <|
                                                        (FontAwesome.Solid.chevronUp
                                                            |> FontAwesome.Icon.present
                                                            |> FontAwesome.Icon.styled [ FontAwesome.Attributes.sm ]
                                                            |> FontAwesome.Icon.view
                                                        )
                                                    , html <|
                                                        (FontAwesome.Solid.chevronDown
                                                            |> FontAwesome.Icon.present
                                                            |> FontAwesome.Icon.styled [ FontAwesome.Attributes.sm ]
                                                            |> FontAwesome.Icon.view
                                                        )
                                                    ]
                                            ]
                                    }
                            )
                        ]

                    _ ->
                        []
                )
              <|
                el
                    (case actSelection of
                        ActBet _ ->
                            [ transparent True ]

                        _ ->
                            []
                    )
                <|
                    -- only show call when required, show a placeholder instead before betting has started
                    -- or bet amount slider, when betting
                    -- stack at the bottom, bet at the top (both editable text fields)
                    -- as slider between moves, they are updated
                    el (buttonHiddenAttrs <| (not isActive || callAmount <= 0))
                    <|
                        controlsButton Theme.scheme1 (Bet <| min callAmount self.stack) <|
                            column
                                [ spacing 5
                                , width fill
                                ]
                                [ el
                                    [ width fill
                                    , Font.center
                                    ]
                                  <|
                                    if callAmount > self.stack then
                                        text "all-in"

                                    else
                                        text "call"
                                , el
                                    [ Font.size 18
                                    , width fill
                                    , Font.center
                                    ]
                                  <|
                                    text <|
                                        String.fromInt <|
                                            min callAmount self.stack
                                ]
            , row
                [ width <| minimum 130 shrink
                , Font.size 22
                ]
                [ el
                    [ centerX
                    ]
                  <|
                    text <|
                        String.fromInt self.stack
                , text " "
                , html
                    -- this middle element becomes cancel when betting
                    (FontAwesome.Solid.caretRight
                        |> FontAwesome.Icon.present
                        |> FontAwesome.Icon.styled [ FontAwesome.Attributes.sm ]
                        |> FontAwesome.Icon.withId "self-controls-bet-amount"
                        |> FontAwesome.Icon.view
                    )
                , text " "
                , el
                    [ centerX ]
                  <|
                    if self.bet > 0 then
                        text <|
                            String.fromInt self.bet

                    else
                        text "0"
                ]
            , el
                []
              <|
                case actSelection of
                    ActBet betAmount ->
                        el (buttonHiddenAttrs <| not isActive) <|
                            controlsButton Theme.scheme1 (Bet betAmount) <|
                                column
                                    [ width fill
                                    , spacing 5
                                    ]
                                    [ el
                                        [ width fill
                                        , Font.center
                                        ]
                                      <|
                                        text <|
                                            if highestBet > 0 then
                                                "raise"

                                            else
                                                "bet"
                                    , el
                                        [ width fill
                                        , Font.center
                                        , Font.size 18
                                        ]
                                      <|
                                        text <|
                                            if highestBet > 0 then
                                                "to " ++ String.fromInt (betAmount + self.bet)

                                            else
                                                String.fromInt (betAmount + self.bet)
                                    ]

                    _ ->
                        el (buttonHiddenAttrs <| not isActive) <|
                            controlsButton Theme.scheme1 (InputActSelection <| ActBet currentSelectedBetAmount) <|
                                text <|
                                    if highestBet > 0 then
                                        "raise"

                                    else
                                        "bet"
            ]
        , el
            [ centerX ]
          <|
            case actSelection of
                ActBet _ ->
                    el (buttonHiddenAttrs <| not isActive) <|
                        controlsButton Theme.scheme1 (InputActSelection NoAct) <|
                            column
                                [ width fill
                                , spacing 5
                                ]
                                [ el
                                    [ width fill
                                    , Font.center
                                    ]
                                  <|
                                    text "cancel"
                                , el
                                    [ centerX
                                    , Font.center
                                    ]
                                  <|
                                    html
                                        (FontAwesome.Solid.times
                                            |> FontAwesome.Icon.present
                                            |> FontAwesome.Icon.styled [ FontAwesome.Attributes.sm ]
                                            |> FontAwesome.Icon.withId "cancel-bet_pokerdot"
                                            |> FontAwesome.Icon.titled "cancel bet"
                                            |> FontAwesome.Icon.view
                                        )
                                ]

                _ ->
                    el (buttonHiddenAttrs <| (not isActive || self.bet < highestBet)) <|
                        controlsButton Theme.scheme1 Check <|
                            text "check"
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
        [ height <| px 88
        , width <| px 88
        , Border.rounded (88 // 2)
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

                LargeCard ->
                    ( 88, 25, 20 )
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

                LargeCard ->
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


handUi : Viewport -> String -> Int -> Maybe ( Card, Card ) -> Hand -> Element Msg
handUi viewport name winnings maybeHole hand =
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

        scheme =
            Theme.scheme3
    in
    el
        [ width fill
        , Background.color scheme.main
        ]
    <|
        container viewport <|
            column
                [ width fill
                , spacing 5
                , paddingXY 0 10
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
                        , Font.color <| Theme.textColour scheme.highlight
                        , Font.shadow
                            { offset = ( 1, 1 )
                            , blur = 0.5
                            , color = Theme.glow scheme.highlight
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
                , row
                    [ width fill
                    , spacing cardSpacing
                    ]
                  <|
                    List.append cardEls
                        [ el
                            [ alignRight
                            , Font.size 20
                            , Font.color <| Theme.textColour Theme.colours.white
                            , Font.shadow
                                { offset = ( 1, 1 )
                                , blur = 0.5
                                , color = Theme.glow scheme.highlight
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
                                [ alignRight ]
                                [ Element.html <|
                                    (winningsIcon
                                        |> FontAwesome.Icon.present
                                        |> FontAwesome.Icon.styled [ FontAwesome.Attributes.sm ]
                                        |> FontAwesome.Icon.withId ("hand-ui-winnings_pokerdot_" ++ name)
                                        |> FontAwesome.Icon.titled "Winnings"
                                        |> FontAwesome.Icon.view
                                    )
                                , text <| String.fromInt winnings
                                ]
                        ]
                , el
                    [ Font.size 15
                    , Font.color <| Theme.textColour Theme.colours.white
                    , Font.shadow
                        { offset = ( 1, 1 )
                        , blur = 0.5
                        , color = Theme.glow scheme.highlight
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


uiElements : Model -> Int -> ActSelection -> Element Msg
uiElements model seed act =
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
            Random.Extra.map6
                (\name stack pot bet c1 c2 ->
                    { playerId = Pid "player-id"
                    , screenName = name
                    , isAdmin = False
                    , isHost = False
                    , stack = stack
                    , pot = pot
                    , bet = bet
                    , folded = False
                    , busted = False
                    , hole = Just ( c1, c2 )
                    }
                )
                nameGen
                (Random.int 0 1000)
                winningGen
                winningGen
                cardGen
                cardGen

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

        ( { holes, players, round, inTurn }, seed3 ) =
            Random.step
                (Random.map4
                    (\h ps r b ->
                        { holes = h
                        , players = ps
                        , round = r
                        , inTurn = b
                        }
                    )
                    holesGen
                    (Random.list 5 playerGen)
                    roundGen
                    Random.Extra.bool
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
        , paddingEach { zWidths | top = 50 }
        ]
        [ container model.viewport <|
            row
                [ width fill
                , spacing 25
                ]
                [ Input.button
                    [ height <| px 40
                    , width <| px 40
                    ]
                    { onPress = Just (NavigateUIElements <| seed - 1)
                    , label =
                        html
                            (FontAwesome.Solid.minusSquare
                                |> FontAwesome.Icon.present
                                |> FontAwesome.Icon.styled [ FontAwesome.Attributes.fa2x ]
                                |> FontAwesome.Icon.view
                            )
                    }
                , Input.slider
                    [ height <| px 40
                    , width fill
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
                            (el
                                [ width fill
                                , Font.center
                                ]
                             <|
                                text <|
                                    "Seed "
                                        ++ String.fromInt seed
                            )
                    , min = 0
                    , max = 400
                    , step = Nothing
                    , value = toFloat seed
                    , thumb =
                        Input.defaultThumb
                    }
                , Input.button
                    [ height <| px 40
                    , width <| px 40
                    ]
                    { onPress = Just (NavigateUIElements <| seed + 1)
                    , label =
                        html
                            (FontAwesome.Solid.plusSquare
                                |> FontAwesome.Icon.present
                                |> FontAwesome.Icon.styled [ FontAwesome.Attributes.fa2x ]
                                |> FontAwesome.Icon.view
                            )
                    }
                ]
        , container model.viewport <|
            tableUi round
                [ getPlayer 0
                , getPlayer 1
                , getPlayer 2
                , getPlayer 3
                , getPlayer 4
                ]
        , container model.viewport <|
            selfUi model.peeking <|
                getPlayer 0
        , container model.viewport <|
            pokerControlsUi inTurn 10 act (getPlayer 0) <|
                List.map getPlayer [ 1, 2, 3, 4 ]
        , handUi model.viewport (getName 0) (getWinnings 0) (getHole 0) hands.highCard
        , handUi model.viewport (getName 1) (getWinnings 1) (getHole 1) hands.pair
        , handUi model.viewport (getName 2) (getWinnings 2) (getHole 2) hands.twoPair
        , handUi model.viewport (getName 3) (getWinnings 3) (getHole 3) hands.threeOfAKind
        , handUi model.viewport (getName 4) (getWinnings 4) (getHole 4) hands.straight
        , handUi model.viewport (getName 5) (getWinnings 5) (getHole 5) hands.flush
        , handUi model.viewport (getName 6) (getWinnings 6) (getHole 6) hands.fullHouse
        , handUi model.viewport (getName 7) (getWinnings 7) (getHole 7) hands.fourOfAKind
        , handUi model.viewport (getName 8) (getWinnings 8) (getHole 8) hands.straight
        , handUi model.viewport (getName 9) (getWinnings 9) (getHole 9) hands.straightFlush
        , handUi model.viewport (getName 10) (getWinnings 10) (getHole 10) hands.royalFlush
        , container model.viewport <|
            communityCardsUi flopRound
        , container model.viewport <|
            communityCardsUi turnRound
        , container model.viewport <|
            communityCardsUi riverRound
        ]
