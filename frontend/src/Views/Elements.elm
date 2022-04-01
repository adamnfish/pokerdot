module Views.Elements exposing (blindUi, buttonHiddenAttrs, cardUi, communityCardsUi, connectionUi, container, controlsButton, divider, editTimerUi, formatBlinds, formatTimeComponent, handUi, helpText, hiddenCardUi, logo, pdTab, pdText, pokerControlsUi, pokerdotInText, rejoinFromLibraryUi, rgbToStyle, selfUi, tableUi, timerLevelUi, uiElements, zWidths)

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
import Html
import Html.Attributes
import List.Extra
import Logic exposing (isBusted)
import Maybe.Extra
import Model
    exposing
        ( ActSelection(..)
        , Card
        , ChipsSettings(..)
        , EditBlindsSettings(..)
        , Game
        , Hand(..)
        , Model
        , Msg(..)
        , Player
        , PlayerId(..)
        , PlayerWinnings
        , Rank(..)
        , Round(..)
        , Self
        , TimerLevel(..)
        , TimerStatus
        , Welcome
        )
import Random
import Random.Extra
import Svg
import Svg.Attributes
import Time exposing (Posix, millisToPosix, posixToMillis)
import Timers
    exposing
        ( CurrentTimerLevel(..)
        , TimerSpeed(..)
        , currentTimerLevel
        , filteredTimerLevels
        , timerRecommendations
        )
import Utils exposing (TimeComponents, millisToTimeComponents)
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
        , mouseOver
            [ Background.color <| Theme.focusColour scheme.highlight
            ]
        ]
        { onPress = Just msg
        , label = label
        }


to255 : Float -> Int
to255 f =
    round <| f * 255


rgbToStyle : { red : Float, green : Float, blue : Float, alpha : Float } -> String
rgbToStyle rgb =
    "rgb("
        ++ String.fromInt (to255 rgb.red)
        ++ ","
        ++ String.fromInt (to255 rgb.green)
        ++ ","
        ++ String.fromInt (to255 rgb.blue)
        ++ ")"


logo : Int -> Element Msg
logo dimensions =
    html <|
        Svg.svg
            [ Svg.Attributes.width <| String.fromInt dimensions
            , Svg.Attributes.height <| String.fromInt dimensions
            , Svg.Attributes.viewBox "0 0 800 800"
            ]
            [ Svg.path
                [ Svg.Attributes.fill <| rgbToStyle <| Element.toRgb Theme.colours.highlightPrimary
                , Svg.Attributes.d "M 350 75 A 275 275 0 0 0 75 350 L 75 800 L 175 800 L 175 561.97461 A 275 275 0 0 0 350 625 A 275 275 0 0 0 625 350 A 275 275 0 0 0 350 75 z "
                ]
                []
            , Svg.path
                [ Svg.Attributes.fill <| rgbToStyle <| Element.toRgb Theme.colours.lowlight
                , Svg.Attributes.opacity "0.8"
                , Svg.Attributes.d "M 625 0 L 625 238.02539 A 275 275 0 0 0 450 175 A 275 275 0 0 0 175 450 A 275 275 0 0 0 450 725 A 275 275 0 0 0 725 450 L 725 0 L 625 0 z "
                ]
                []
            ]


divider : Element Msg
divider =
    column
        [ width fill
        , padding 8
        ]
        [ html <|
            Html.hr [ Html.Attributes.hidden True ] []
        , el
            [ width fill
            , height <| px 1
            , Background.color Theme.colours.highlightPrimary
            ]
          <|
            Element.none
        , el
            [ width fill
            , height <| px 1
            , Background.color Theme.colours.lowlight
            ]
          <|
            Element.none
        ]


pokerdotInText : Element Msg
pokerdotInText =
    el [ Font.color Theme.colours.lowlight ] <| text "pokerdot"


helpText : List String -> Element Msg
helpText helpStrs =
    let
        helpFragment str =
            el
                [ padding 2
                , Background.color Theme.colours.highlightPrimary
                , Font.color Theme.colours.lowlight
                ]
            <|
                text str
    in
    paragraph
        [ paddingXY 0 25
        , spacing 12
        , Font.alignLeft
        , Font.size 18
        ]
    <|
        List.map helpFragment helpStrs


pdTab : Color -> Bool -> Msg -> String -> Element Msg
pdTab tabColour active msg label =
    if active then
        el
            [ paddingXY 8 5
            , Border.rounded 1
            , Border.solid
            , Border.width 2
            , Border.color Theme.colours.night
            , Border.shadow
                { offset = ( 2, 2 )
                , size = 0
                , blur = 0
                , color = Theme.dim Theme.colours.shadow
                }
            , Background.color Theme.colours.lowlight
            , Font.color <| Theme.textColour Theme.colours.white
            ]
        <|
            text label

    else
        Input.button
            [ paddingXY 8 5
            , Border.rounded 1
            , Border.solid
            , Border.width 2
            , Border.color Theme.colours.black
            , Border.shadow
                { offset = ( 5, 5 )
                , size = 0
                , blur = 0
                , color = Theme.glow tabColour
                }
            , Background.color tabColour
            , Font.color <| Theme.textColour Theme.colours.white
            , focused
                [ Background.color <| Theme.focusColour Theme.colours.highlightSecondary
                , Border.color Theme.colours.white
                , Font.color <| Theme.textColour Theme.colours.white
                ]
            , mouseOver
                [ Background.color <| Theme.focusColour Theme.colours.highlightSecondary
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


rejoinFromLibraryUi : Posix -> List Welcome -> Element Msg
rejoinFromLibraryUi now library =
    let
        filterByTimeThresholds low high =
            List.filter
                (\welcome ->
                    let
                        ago =
                            posixToMillis now - posixToMillis welcome.joined
                    in
                    ago >= low && ago < high
                )
                library

        section message welcomes =
            if List.isEmpty welcomes then
                Element.none

            else
                column
                    [ width fill
                    , spacing 8
                    ]
                    [ el
                        [ paddingEach
                            { left = 48
                            , top = 0
                            , bottom = 0
                            , right = 0
                            }
                        ]
                      <|
                        text message
                    , column
                        [ width fill
                        , spacing 15
                        ]
                      <|
                        List.map rejoinGameButton welcomes
                    ]
    in
    column
        [ width fill
        , spacing 20
        ]
        [ section "just now" <|
            filterByTimeThresholds
                0
                (1000 * 59)
        , section "a minute ago" <|
            filterByTimeThresholds
                (1000 * 59)
                (1000 * 119)
        , section "a few minutes ago" <|
            filterByTimeThresholds
                (1000 * 119)
                (1000 * 60 * 50)
        , section "an hour ago" <|
            filterByTimeThresholds
                (1000 * 60 * 50)
                (1000 * 60 * 100)
        , section "a few hours ago" <|
            filterByTimeThresholds
                (1000 * 60 * 100)
                (1000 * 60 * 60 * 18)
        , section "yesterday" <|
            filterByTimeThresholds
                (1000 * 60 * 60 * 18)
                (1000 * 60 * 60 * 36)
        , section "a few days ago" <|
            filterByTimeThresholds
                (1000 * 60 * 60 * 36)
                (1000 * 60 * 60 * 24 * 5)
        , section "last week" <|
            filterByTimeThresholds
                (1000 * 60 * 60 * 24 * 5)
                (1000 * 60 * 60 * 24 * 12)
        , section "a few weeks back" <|
            List.filter
                (\welcome ->
                    let
                        ago =
                            posixToMillis welcome.joined - posixToMillis now
                    in
                    ago >= 1000 * 60 * 60 * 24 * 12
                )
                library
        ]


rejoinGameButton welcomeMessage =
    row
        [ width fill
        , spacing 6
        ]
        [ Input.button
            [ height <| px 40
            , width <| px 40
            , centerY
            , alignRight
            , Border.solid
            , Border.width 2
            , Border.rounded 2
            , Border.color Theme.colours.black
            , Border.shadow
                { offset = ( 5, 5 )
                , size = 0
                , blur = 0
                , color = Theme.glow Theme.colours.error
                }
            , Background.color Theme.colours.error
            , focused
                [ Background.color <| Theme.focusColour Theme.colours.error
                , Border.color Theme.colours.white
                , Font.color Theme.colours.white
                ]
            , mouseOver
                [ Background.color <| Theme.focusColour Theme.colours.error ]
            ]
            { onPress = Just <| DeletePersistedGame welcomeMessage
            , label =
                el
                    [ centerX
                    , centerY
                    ]
                <|
                    html <|
                        (FontAwesome.Solid.times
                            |> FontAwesome.Icon.present
                            |> FontAwesome.Icon.styled
                                [ FontAwesome.Attributes.sm
                                , FontAwesome.Attributes.fw
                                ]
                            |> FontAwesome.Icon.withId ("pokerdot_library-rejoin-dismiss-" ++ welcomeMessage.gameCode)
                            |> FontAwesome.Icon.titled "Remove game"
                            |> FontAwesome.Icon.view
                        )
            }
        , Input.button
            [ width fill
            , padding 5
            , Border.solid
            , Border.width 2
            , Border.rounded 2
            , Border.color Theme.colours.black
            , Border.shadow
                { offset = ( 5, 5 )
                , size = 0
                , blur = 0
                , color = Theme.glow Theme.scheme2.main
                }
            , Background.color Theme.scheme2.main
            , Font.color Theme.colours.white
            , focused
                [ Background.color <| Theme.focusColour Theme.scheme2.main
                , Border.color Theme.colours.white
                ]
            , mouseOver
                [ Background.color <| Theme.focusColour Theme.scheme2.main ]
            ]
            { onPress = Just <| NavigateGame welcomeMessage
            , label =
                column
                    [ spacing 5 ]
                    [ row
                        []
                        [ el
                            [ Font.color Theme.scheme2.highlight ]
                          <|
                            html <|
                                (FontAwesome.Solid.user
                                    |> FontAwesome.Icon.present
                                    |> FontAwesome.Icon.styled
                                        [ FontAwesome.Attributes.sm
                                        , FontAwesome.Attributes.fw
                                        ]
                                    |> FontAwesome.Icon.withId ("pokerdot_library-rejoin-player-" ++ welcomeMessage.gameCode)
                                    |> FontAwesome.Icon.titled "Your name"
                                    |> FontAwesome.Icon.view
                                )
                        , text " "
                        , text welcomeMessage.screenName
                        ]
                    , row
                        []
                        [ el
                            [ Font.color Theme.scheme2.highlight ]
                          <|
                            html <|
                                (FontAwesome.Solid.gamepad
                                    |> FontAwesome.Icon.present
                                    |> FontAwesome.Icon.styled
                                        [ FontAwesome.Attributes.sm
                                        , FontAwesome.Attributes.fw
                                        ]
                                    |> FontAwesome.Icon.withId ("pokerdot_library-rejoin-game-" ++ welcomeMessage.gameCode)
                                    |> FontAwesome.Icon.titled "Game name"
                                    |> FontAwesome.Icon.view
                                )
                        , text " "
                        , text welcomeMessage.gameName
                        ]
                    ]
            }
        ]



-- TODO: show empty stack as busted on round results screen
-- TODO: also show pot winnings for player separately here for round results


tableUi : Round -> Int -> Maybe (List PlayerWinnings) -> Maybe PlayerId -> List Player -> Element Msg
tableUi round button maybeWinnings active players =
    let
        pot =
            List.sum <| List.map .pot players

        scheme =
            Theme.scheme1

        dealer =
            List.Extra.getAt button players

        isDealer player =
            Maybe.withDefault False <|
                Maybe.map
                    (\p ->
                        p.playerId == player.playerId
                    )
                    dealer

        isActive player =
            Maybe.withDefault False <|
                Maybe.map
                    (\pid ->
                        pid == player.playerId
                    )
                    active

        totalFunds =
            List.sum <|
                List.map .stack players

        seat : Player -> Element Msg
        seat player =
            let
                winnings =
                    Maybe.withDefault 0 <|
                        Maybe.map
                            (\pw ->
                                pw.winnings
                            )
                        <|
                            Maybe.andThen
                                (List.Extra.find (\pw -> pw.playerId == player.playerId))
                                maybeWinnings

                -- less than half their share of the available funds
                isPoor =
                    player.stack < (totalFunds // (List.length players * 2))

                busted =
                    isBusted round player
            in
            column
                [ width fill
                , spacing 0
                ]
                [ row
                    ([ width fill
                     , spacing 8
                     , padding 8
                     , if busted || player.folded then
                        Background.color Theme.colours.disabled

                       else if isActive player then
                        Background.color Theme.colours.highlightPrimary

                       else
                        Background.color scheme.main
                     ]
                        ++ (if isDealer player then
                                [ inFront <|
                                    el
                                        [ width <| px 18
                                        , height <| px 18
                                        , moveRight 3
                                        , moveDown 9
                                        , Background.color Theme.colours.night
                                        , Border.rounded 9
                                        , Font.size 12
                                        , Font.color <| Theme.textColour Theme.colours.white
                                        , Font.bold
                                        ]
                                    <|
                                        el [ centerX, centerY ] <|
                                            text "d"
                                ]

                            else
                                []
                           )
                    )
                    [ el
                        ([ width fill
                         , Font.alignLeft
                         , Font.color <| Theme.textColour Theme.colours.black
                         , Font.size 18
                         , clip
                         , paddingEach
                            { left = 15
                            , top = 0
                            , bottom = 0
                            , right = 0
                            }
                         ]
                            ++ (if busted || player.folded then
                                    [ Font.strike
                                    ]

                                else
                                    []
                               )
                        )
                      <|
                        text player.screenName
                    , if busted then
                        el
                            [ Font.alignRight
                            , Font.color <| Theme.glow <| Theme.textColour Theme.colours.black
                            ]
                        <|
                            text "busted"

                      else
                        el
                            [ width <| px 50
                            , Font.alignRight
                            , Font.color <| Theme.textColour Theme.colours.black
                            ]
                        <|
                            text <|
                                String.fromInt (player.stack - winnings)
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

                          else if winnings > 0 then
                            html <|
                                (FontAwesome.Solid.caretLeft
                                    |> FontAwesome.Icon.present
                                    |> FontAwesome.Icon.styled [ FontAwesome.Attributes.sm ]
                                    |> FontAwesome.Icon.withId ("table-ui-bet-display_pokerdot_" ++ player.screenName)
                                    |> FontAwesome.Icon.view
                                )

                          else
                            Element.none
                        , text " "
                        , if player.bet > 0 then
                            text <| String.fromInt player.bet

                          else if winnings > 0 then
                            text <| String.fromInt winnings

                          else
                            Element.none
                        ]
                    ]
                , row
                    [ width fill
                    , padding 0
                    ]
                    [ el
                        [ width <| fillPortion player.stack
                        , height <| px 2
                        , Background.color <|
                            if isPoor then
                                Theme.colours.error

                            else
                                Theme.colours.icon
                        , htmlAttribute <| Html.Attributes.style "transition" "flex-grow 0.5s ease-out"
                        ]
                        Element.none
                    , el
                        [ width <| fillPortion (totalFunds - player.stack)
                        ]
                        Element.none
                    ]
                ]
    in
    column
        [ width fill
        , padding 8
        , spacing 6
        , Background.color Theme.colours.lowlight
        ]
        [ column
            [ width fill
            , spacing 4
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
            [ helpText [ "you are busted" ] ]

    else
        row
            [ width fill ]
            [ case self.hole of
                Nothing ->
                    text " - "

                Just ( card1, card2 ) ->
                    row
                        [ alignLeft
                        , spacing 6
                        ]
                    <|
                        if isPeeking then
                            [ cardUi 0 False LargeCard card1
                            , cardUi 0 False LargeCard card2
                            ]

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
                        [ row []
                            [ text "peek"
                            ]
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
                            (column
                                (List.append
                                    [ width fill
                                    , spacing 8
                                    , moveUp 105
                                    , moveLeft 5
                                    ]
                                    (buttonHiddenAttrs <| not isActive)
                                )
                                [ row
                                    [ width fill
                                    , centerX
                                    , spacing 4
                                    ]
                                    [ Input.button
                                        [ height <| px 45
                                        , width <| px 45
                                        , Font.color <| Theme.textColour Theme.colours.white
                                        , Background.color Theme.scheme3.highlight
                                        , Border.rounded 2
                                        , Border.width 2
                                        , Border.color Theme.colours.black
                                        , focused
                                            [ Background.color <| Theme.focusColour Theme.scheme3.highlight
                                            , Border.color Theme.colours.white
                                            , Border.shadow
                                                { offset = ( 5, 5 )
                                                , size = 0
                                                , blur = 0
                                                , color = Theme.glow <| Theme.focusColour Theme.scheme3.highlight
                                                }
                                            ]
                                        , mouseOver
                                            [ Background.color <| Theme.focusColour Theme.scheme3.highlight ]
                                        ]
                                        { onPress = Just (InputBet <| max 1 (betAmount - 1))
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
                                    , Input.button
                                        [ height <| px 45
                                        , width <| px 45
                                        , Font.color <| Theme.textColour Theme.colours.white
                                        , Background.color Theme.scheme3.highlight
                                        , Border.rounded 2
                                        , Border.width 2
                                        , Border.color Theme.colours.black
                                        , focused
                                            [ Background.color <| Theme.focusColour Theme.scheme3.highlight
                                            , Border.color Theme.colours.white
                                            , Border.shadow
                                                { offset = ( 5, 5 )
                                                , size = 0
                                                , blur = 0
                                                , color = Theme.glow <| Theme.focusColour Theme.scheme3.highlight
                                                }
                                            ]
                                        , mouseOver
                                            [ Background.color <| Theme.focusColour Theme.scheme3.highlight ]
                                        ]
                                        { onPress = Just (InputBet <| min self.stack (betAmount + 1))
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
                                , el
                                    [ moveRight 22
                                    ]
                                  <|
                                    Input.slider
                                        [ width <| px 50
                                        , height <| px 250
                                        , Element.behindContent
                                            (Element.el
                                                [ width <| px 20
                                                , height fill
                                                , centerX
                                                , Background.color <| Theme.glow Theme.colours.lowlight
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
                                            if highestBet > 0 then
                                                Input.labelHidden "edit raise amount"

                                            else
                                                Input.labelHidden "edit bet amount"
                                        , min = toFloat minimumRaise
                                        , max = toFloat self.stack
                                        , step = Just <| toFloat (smallBlind * 2)
                                        , value = toFloat <| max betAmount minimumRaise
                                        , thumb =
                                            Input.thumb
                                                [ width <| px 50
                                                , height <| px 50
                                                , Background.color Theme.scheme3.highlight
                                                , focused
                                                    [ Background.color <| Theme.focusColour Theme.scheme3.highlight
                                                    , Border.color Theme.colours.white
                                                    , Border.shadow
                                                        { offset = ( 3, 3 )
                                                        , size = 0
                                                        , blur = 0
                                                        , color = Theme.glow Theme.scheme3.highlight
                                                        }
                                                    ]
                                                , mouseOver
                                                    [ Background.color <| Theme.focusColour Theme.scheme3.highlight ]
                                                , Border.solid
                                                , Border.rounded 2
                                                , Border.width 2
                                                , Border.color Theme.colours.black
                                                , Border.shadow
                                                    { offset = ( 3, 3 )
                                                    , size = 0
                                                    , blur = 0
                                                    , color = Theme.glow Theme.scheme3.highlight
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
                                ]
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
                            controlsButton Theme.scheme3 (InputActSelection <| ActBet currentSelectedBetAmount) <|
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
                        controlsButton Theme.scheme3 (InputActSelection NoAct) <|
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
    el
        [ height <| px 88
        , width <| px 88
        , Border.rounded (88 // 2)
        , Background.color bgColour
        , Border.width 1
        , Border.color <| Theme.colours.lowlight
        , clip
        ]
    <|
        el
            [ height <| px 86
            , width <| px 86
            , Border.rounded (86 // 2)
            , Background.color bgColour
            , Border.width 1
            , Border.color <| Theme.colours.highlightPrimary
            , clip
            , Font.size 25
            , Font.color textColour
            ]
        <|
            row
                [ centerX
                , centerY
                ]
                [ logo 50
                ]


cardUi : Int -> Bool -> CardSize -> Card -> Element Msg
cardUi offsetIndex highlight cardSize card =
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
                    , Theme.colours.night
                    , rgb255 253 255 255
                    )

                Model.Diamonds ->
                    ( text "♦"
                    , Theme.slightlyDim Theme.colours.error
                    , rgb255 255 253 253
                    )

                Model.Spades ->
                    ( text "♠"
                    , Theme.colours.night
                    , rgb255 253 255 253
                    )

                Model.Hearts ->
                    ( text "♥"
                    , Theme.slightlyDim Theme.colours.error
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
        ([ height <| px radius
         , width <| px radius
         , Border.rounded (radius // 2)
         , Background.color bgColour
         , Border.width <|
            case cardSize of
                SmallCard ->
                    1

                NormalCard ->
                    1

                LargeCard ->
                    1
         , Border.color textColour
         , clip
         , Font.size fontSize
         , Font.color textColour
         , moveLeft <| toFloat offsetIndex * offset

         --, Background.color <| rgb255 200 200 200
         ]
            ++ (if highlight then
                    [ moveUp 4
                    , Border.shadow
                        { offset = ( 0, 4 )
                        , size = 0
                        , blur = 0
                        , color = Theme.pressColour Theme.colours.lowlight
                        }
                    ]

                else
                    []
               )
        )
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

        isHoleCard card =
            Maybe.withDefault False <|
                Maybe.map
                    (\( c1, c2 ) ->
                        card == c1 || card == c2
                    )
                    maybeHole

        ( label, cardEls ) =
            case hand of
                HighCard c1 c2 c3 c4 c5 ->
                    ( "HIGH CARD"
                    , [ cardUi 0 (isHoleCard c1) NormalCard c1
                      , cardUi 0 (isHoleCard c2) NormalCard c2
                      , cardUi 0 (isHoleCard c3) NormalCard c3
                      , cardUi 0 (isHoleCard c4) NormalCard c4
                      , cardUi 0 (isHoleCard c5) NormalCard c5
                      ]
                    )

                Pair p1 p2 k1 k2 k3 ->
                    ( "PAIR"
                    , [ cardUi 0 (isHoleCard p1) NormalCard p1
                      , cardUi 1 (isHoleCard p2) NormalCard p2
                      , cardUi 0 (isHoleCard k1) NormalCard k1
                      , cardUi 0 (isHoleCard k2) NormalCard k2
                      , cardUi 0 (isHoleCard k3) NormalCard k3
                      ]
                    )

                TwoPair p11 p12 p21 p22 k ->
                    ( "TWO PAIR"
                    , [ cardUi 0 (isHoleCard p11) NormalCard p11
                      , cardUi 1 (isHoleCard p12) NormalCard p12
                      , cardUi 0 (isHoleCard p21) NormalCard p21
                      , cardUi 1 (isHoleCard p22) NormalCard p22
                      , cardUi 0 (isHoleCard k) NormalCard k
                      ]
                    )

                ThreeOfAKind t1 t2 t3 k1 k2 ->
                    ( "THREE OF A KIND"
                    , [ cardUi 0 (isHoleCard t1) NormalCard t1
                      , cardUi 1 (isHoleCard t2) NormalCard t2
                      , cardUi 2 (isHoleCard t3) NormalCard t3
                      , cardUi 0 (isHoleCard k1) NormalCard k1
                      , cardUi 0 (isHoleCard k2) NormalCard k2
                      ]
                    )

                Straight c1 c2 c3 c4 c5 ->
                    ( "STRAIGHT"
                    , [ cardUi 0 (isHoleCard c1) NormalCard c1
                      , cardUi 1 (isHoleCard c2) NormalCard c2
                      , cardUi 2 (isHoleCard c3) NormalCard c3
                      , cardUi 3 (isHoleCard c4) NormalCard c4
                      , cardUi 4 (isHoleCard c5) NormalCard c5
                      ]
                    )

                Flush c1 c2 c3 c4 c5 ->
                    ( "FLUSH"
                    , [ cardUi 0 (isHoleCard c1) NormalCard c1
                      , cardUi 1 (isHoleCard c2) NormalCard c2
                      , cardUi 2 (isHoleCard c3) NormalCard c3
                      , cardUi 3 (isHoleCard c4) NormalCard c4
                      , cardUi 4 (isHoleCard c5) NormalCard c5
                      ]
                    )

                FullHouse t1 t2 t3 p1 p2 ->
                    ( "FULL HOUSE"
                    , [ cardUi 0 (isHoleCard t1) NormalCard t1
                      , cardUi 1 (isHoleCard t2) NormalCard t2
                      , cardUi 2 (isHoleCard t3) NormalCard t3
                      , cardUi 0 (isHoleCard p1) NormalCard p1
                      , cardUi 1 (isHoleCard p2) NormalCard p2
                      ]
                    )

                FourOfAKind q1 q2 q3 q4 k ->
                    ( "FOUR OF A KIND"
                    , [ cardUi 0 (isHoleCard q1) NormalCard q1
                      , cardUi 1 (isHoleCard q2) NormalCard q2
                      , cardUi 2 (isHoleCard q3) NormalCard q3
                      , cardUi 3 (isHoleCard q4) NormalCard q4
                      , cardUi 0 (isHoleCard k) NormalCard k
                      ]
                    )

                StraightFlush c1 c2 c3 c4 c5 ->
                    ( if c1.rank == Ten then
                        "ROYAL FLUSH"

                      else
                        "STRAIGHT FLUSH"
                    , [ cardUi 0 (isHoleCard c1) NormalCard c1
                      , cardUi 1 (isHoleCard c2) NormalCard c2
                      , cardUi 2 (isHoleCard c3) NormalCard c3
                      , cardUi 3 (isHoleCard c4) NormalCard c4
                      , cardUi 4 (isHoleCard c5) NormalCard c5
                      ]
                    )

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
                                [ cardUi 0 False SmallCard card1
                                , cardUi 1 False SmallCard card2
                                ]

                        Nothing ->
                            Element.none
                    , el
                        [ width fill
                        , clip
                        , Font.size 20
                        , Font.alignLeft
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
                        text name
                    ]
                , row
                    [ width fill
                    , spacing cardSpacing
                    ]
                  <|
                    List.append cardEls
                        [ el
                            [ width fill
                            , alignRight
                            , Font.size 20
                            , Font.color <| Theme.textColour Theme.colours.primary
                            , Font.shadow
                                { offset = ( 1, 1 )
                                , blur = 0.5
                                , color = Theme.glow Theme.colours.lowlight
                                }
                            , paddingEach
                                { top = 0
                                , bottom = 2
                                , left = 10
                                , right = 10
                                }
                            ]
                          <|
                            if winnings > 0 then
                                column
                                    [ width <| px 66
                                    , alignRight
                                    , spacing 2
                                    ]
                                    [ el
                                        [ centerX ]
                                      <|
                                        Element.html <|
                                            (FontAwesome.Solid.crown
                                                |> FontAwesome.Icon.present
                                                |> FontAwesome.Icon.styled [ FontAwesome.Attributes.sm ]
                                                |> FontAwesome.Icon.withId ("hand-ui-winnings_pokerdot_" ++ name)
                                                |> FontAwesome.Icon.titled "winnings"
                                                |> FontAwesome.Icon.view
                                            )
                                    , el [ centerX ] <| text <| String.fromInt winnings
                                    ]

                            else
                                el
                                    [ width <| px 66
                                    , alignRight
                                    ]
                                <|
                                    el [ centerX ] <|
                                        Element.html <|
                                            (FontAwesome.Regular.timesCircle
                                                |> FontAwesome.Icon.present
                                                |> FontAwesome.Icon.styled [ FontAwesome.Attributes.sm ]
                                                |> FontAwesome.Icon.withId ("hand-ui-lost_pokerdot_" ++ name)
                                                |> FontAwesome.Icon.titled "lost this hand"
                                                |> FontAwesome.Icon.view
                                            )
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
                [ cardUi 0 False NormalCard flop1
                , cardUi 1 False NormalCard flop2
                , cardUi 2 False NormalCard flop3
                ]

            TurnRound flop1 flop2 flop3 turn ->
                [ cardUi 0 False NormalCard flop1
                , cardUi 1 False NormalCard flop2
                , cardUi 2 False NormalCard flop3
                , cardUi 3 False NormalCard turn
                ]

            RiverRound flop1 flop2 flop3 turn river ->
                [ cardUi 0 False NormalCard flop1
                , cardUi 1 False NormalCard flop2
                , cardUi 2 False NormalCard flop3
                , cardUi 3 False NormalCard turn
                , cardUi 4 False NormalCard river
                ]

            ShowdownRound flop1 flop2 flop3 turn river _ ->
                [ cardUi 0 False NormalCard flop1
                , cardUi 1 False NormalCard flop2
                , cardUi 2 False NormalCard flop3
                , cardUi 3 False NormalCard turn
                , cardUi 4 False NormalCard river
                ]


timerLevelUi : TimerLevel -> Element Msg
timerLevelUi timerLevel =
    -- TODO: controls for deletion, addition and movement
    let
        durationEl duration =
            let
                timeComponents =
                    millisToTimeComponents (duration * 1000)
            in
            -- TODO: human formatting of times
            row
                [ width fill
                , spacing 2
                ]
                [ formatTimeComponent timeComponents.minutes "m"
                , formatTimeComponent timeComponents.seconds "s"
                ]
    in
    case timerLevel of
        RoundLevel duration smallBlind ->
            row
                [ width fill
                , spacing 8
                , padding 8
                , Background.color Theme.colours.primary
                , Font.color <| Theme.textColour Theme.colours.black
                ]
                [ el
                    [ width <| fillPortion 4 ]
                  <|
                    durationEl duration
                , el
                    []
                  <|
                    text <|
                        formatBlinds smallBlind
                ]

        BreakLevel duration ->
            row
                [ width fill ]
                [ text "break"
                , durationEl duration
                ]


editTimerUi : (Int -> List TimerLevel -> Msg) -> List TimerLevel -> Int -> Int -> Element Msg
editTimerUi msg timerLevels playerCount stack =
    -- show recommendation buttons
    -- and current choice
    -- controls to change all round lengths at once
    let
        largeStartingStack =
            1000

        smallStartingStack =
            100

        longGameTimer =
            timerRecommendations LongGame playerCount largeStartingStack

        mediumGameTimer =
            timerRecommendations MediumGame playerCount largeStartingStack

        shortGameTimer =
            timerRecommendations ShortGame playerCount smallStartingStack
    in
    column
        [ width fill
        , spacing 12
        ]
        [ wrappedRow
            [ width fill
            , spacing 8
            ]
            [ controlsButton Theme.scheme1 (msg smallStartingStack shortGameTimer) <|
                column
                    [ spacing 5
                    , width fill
                    ]
                    [ el
                        [ width fill
                        , Font.center
                        ]
                      <|
                        text "short"
                    , el
                        [ width fill
                        , Font.center
                        ]
                      <|
                        text "game"
                    ]
            , controlsButton Theme.scheme1 (msg largeStartingStack mediumGameTimer) <|
                column
                    [ spacing 5
                    , width fill
                    ]
                    [ el
                        [ width fill
                        , Font.center
                        ]
                      <|
                        text "medium"
                    , el
                        [ width fill
                        , Font.center
                        ]
                      <|
                        text "game"
                    ]
            , controlsButton Theme.scheme1 (msg largeStartingStack longGameTimer) <|
                column
                    [ spacing 5
                    , width fill
                    ]
                    [ el
                        [ width fill
                        , Font.center
                        ]
                      <|
                        text "long"
                    , el
                        [ width fill
                        , Font.center
                        ]
                      <|
                        text "game"
                    ]
            ]
        , el
            []
            Element.none
        , row
            [ width fill
            , padding 2
            , Background.color Theme.colours.night
            , Font.color <| Theme.textColour Theme.colours.white
            ]
            [ el
                [ paddingXY 8 0 ]
              <|
                text "player starting stacks "
            , el
                [ padding 8
                , alignRight
                , Background.color Theme.colours.primary
                , Font.color <| Theme.textColour Theme.colours.black
                ]
              <|
                text <|
                    String.fromInt stack
            ]
        , column
            [ width fill
            , spacing 2
            ]
          <|
            List.map timerLevelUi <|
                filteredTimerLevels stack playerCount timerLevels
        ]


blindUi : Posix -> Maybe TimerStatus -> Int -> Bool -> Element Msg
blindUi now maybeTimerStatus smallBlind isAdmin =
    let
        openBlindOverlayButton =
            Input.button
                [ height <| px 30
                , width <| px 72
                , Font.color <| Theme.textColour Theme.colours.white
                , Font.size 16
                , Background.color Theme.scheme3.highlight
                , Border.rounded 2
                , Border.width 2
                , Border.color Theme.colours.black
                , Border.shadow
                    { offset = ( 5, 5 )
                    , size = 0
                    , blur = 0
                    , color = Theme.glow Theme.scheme3.highlight
                    }
                , focused
                    [ Background.color <| Theme.focusColour Theme.scheme3.highlight
                    , Border.color Theme.colours.white
                    , Border.shadow
                        { offset = ( 5, 5 )
                        , size = 0
                        , blur = 0
                        , color = Theme.glow <| Theme.focusColour Theme.scheme3.highlight
                        }
                    ]
                , mouseOver
                    [ Background.color <| Theme.focusColour Theme.scheme3.highlight ]
                ]
                { onPress = Just OpenEditBlindOverlay
                , label =
                    row
                        [ centerX
                        , centerY
                        , spacing 4
                        ]
                        [ row []
                            [ text "edit" ]
                        , text ""
                        , html
                            (FontAwesome.Solid.edit
                                |> FontAwesome.Icon.present
                                |> FontAwesome.Icon.styled [ FontAwesome.Attributes.xs ]
                                |> FontAwesome.Icon.view
                            )
                        ]
                }

        remaining currentLevelInfo =
            let
                timeComponents =
                    millisToTimeComponents <| 1000 * (currentLevelInfo.levelDuration - currentLevelInfo.levelProgress)
            in
            row
                [ spacing 2 ]
                [ if timeComponents.hours > 0 then
                    formatTimeComponent timeComponents.hours "h"

                  else
                    Element.none
                , if timeComponents.minutes > 0 then
                    formatTimeComponent timeComponents.minutes "m"

                  else
                    Element.none
                , if timeComponents.hours == 0 then
                    formatTimeComponent timeComponents.seconds "s"

                  else
                    Element.none
                ]

        formatMaybeNext : Maybe TimerLevel -> String
        formatMaybeNext maybeNext =
            case maybeNext of
                Just (RoundLevel _ currentSmallBlind) ->
                    formatBlinds currentSmallBlind

                Just (BreakLevel _) ->
                    "break"

                Nothing ->
                    "-"

        timerTemplate : CurrentTimerLevel -> Element Msg
        timerTemplate currentTimerLevel =
            let
                ( progress, label ) =
                    case currentTimerLevel of
                        TimerRunning currentLevelInfo maybeNext ->
                            ( toFloat currentLevelInfo.levelProgress / toFloat currentLevelInfo.levelDuration
                            , remaining currentLevelInfo
                            )

                        TimerBreak currentLevelInfo maybeNext ->
                            ( toFloat currentLevelInfo.levelProgress / toFloat currentLevelInfo.levelDuration
                            , remaining currentLevelInfo
                            )

                        TimerFinished currentSmallBlind ->
                            ( 0
                            , text "-"
                            )

                        TimerPaused currentLevelInfo maybeNext ->
                            ( toFloat currentLevelInfo.levelProgress / toFloat currentLevelInfo.levelDuration
                            , html
                                (FontAwesome.Solid.pause
                                    |> FontAwesome.Icon.present
                                    |> FontAwesome.Icon.styled [ FontAwesome.Attributes.lg ]
                                    |> FontAwesome.Icon.view
                                )
                            )

                        TimerPausedBreak currentLevelInfo maybeNext ->
                            ( toFloat currentLevelInfo.levelProgress / toFloat currentLevelInfo.levelDuration
                            , html
                                (FontAwesome.Solid.pause
                                    |> FontAwesome.Icon.present
                                    |> FontAwesome.Icon.styled [ FontAwesome.Attributes.lg ]
                                    |> FontAwesome.Icon.view
                                )
                            )

                        TimerPausedFinish currentSmallBlind ->
                            ( 0
                            , html
                                (FontAwesome.Solid.pause
                                    |> FontAwesome.Icon.present
                                    |> FontAwesome.Icon.styled [ FontAwesome.Attributes.lg ]
                                    |> FontAwesome.Icon.view
                                )
                            )
            in
            el
                [ height <| px 83
                , inFront <|
                    el
                        [ width <| px 90
                        , height <| px 90
                        , moveRight 4
                        , moveUp 2
                        , alignRight
                        , centerY
                        , behindContent <|
                            el
                                [ width fill
                                , height fill
                                ]
                            <|
                                radialProgressUi
                                    { radius = 45
                                    , progress = progress
                                    , stroke = 4
                                    , progressColour = Theme.colours.icon
                                    , incompleteColour = Theme.colours.lowlight
                                    , fillColour = Theme.colours.highlightPrimary
                                    }
                        ]
                    <|
                        el
                            [ Font.size 14
                            , centerY
                            , centerX
                            ]
                            label
                ]
            <|
                el
                    [ width fill
                    , height fill
                    , paddingEach
                        { zWidths | right = 40 }
                    ]
                <|
                    column
                        [ width fill
                        , alignBottom
                        ]
                        [ if isAdmin then
                            el
                                [ paddingEach
                                    { zWidths | bottom = 6 }
                                ]
                                openBlindOverlayButton

                          else
                            Element.none
                        , el
                            [ width <| minimum 120 shrink
                            , paddingEach
                                { zWidths
                                    | right = 50
                                    , bottom = 4
                                    , top = 4
                                    , left = 8
                                }
                            , Background.color Theme.colours.highlightPrimary
                            ]
                            (case currentTimerLevel of
                                TimerRunning currentLevelInfo _ ->
                                    text <| formatBlinds currentLevelInfo.smallBlind

                                TimerBreak currentLevelInfo _ ->
                                    text <| formatBlinds currentLevelInfo.smallBlind

                                TimerFinished currentSmallBlind ->
                                    text <| formatBlinds currentSmallBlind

                                TimerPaused currentLevelInfo _ ->
                                    text <| formatBlinds currentLevelInfo.smallBlind

                                TimerPausedBreak currentLevelInfo _ ->
                                    text <| formatBlinds currentLevelInfo.smallBlind

                                TimerPausedFinish currentSmallBlind ->
                                    text <| formatBlinds currentSmallBlind
                            )
                        , el
                            [ width <| minimum 110 shrink
                            , alignRight
                            , paddingEach
                                { zWidths
                                    | right = 50
                                    , bottom = 4
                                    , top = 4
                                    , left = 8
                                }
                            , Font.size 11
                            , Font.color <| Theme.textColour Theme.colours.night
                            , Background.color Theme.colours.highlightSecondary
                            ]
                            (case currentTimerLevel of
                                TimerRunning _ maybeNext ->
                                    text <| formatMaybeNext maybeNext

                                TimerBreak _ maybeNext ->
                                    text <| formatMaybeNext maybeNext

                                TimerFinished _ ->
                                    text "-"

                                TimerPaused _ maybeNext ->
                                    text <| formatMaybeNext maybeNext

                                TimerPausedBreak _ maybeNext ->
                                    text <| formatMaybeNext maybeNext

                                TimerPausedFinish _ ->
                                    text "-"
                            )
                        ]
    in
    case Maybe.map (currentTimerLevel now) maybeTimerStatus of
        Just currentTimer ->
            timerTemplate currentTimer

        Nothing ->
            column
                []
                [ if isAdmin then
                    el
                        [ paddingEach
                            { zWidths
                                | bottom = 6
                                , right = 5
                            }
                        , alignRight
                        ]
                        openBlindOverlayButton

                  else
                    Element.none
                , el
                    [ paddingXY 8 2
                    , Background.color Theme.colours.highlightPrimary
                    ]
                  <|
                    text <|
                        formatBlinds smallBlind
                ]


formatBlinds : Int -> String
formatBlinds blindAmount =
    String.fromInt blindAmount ++ " / " ++ (String.fromInt <| blindAmount * 2)


formatTimeComponent : Int -> String -> Element msg
formatTimeComponent amount label =
    row
        []
        [ el
            [ alignBottom ]
          <|
            text <|
                String.fromInt amount
        , el
            [ alignBottom
            , Font.color <| Theme.textColour Theme.colours.night
            ]
          <|
            text label
        ]


buttonHiddenAttrs hidden =
    if hidden then
        [ transparent True
        , htmlAttribute <| Html.Attributes.style "visibility" "hidden"
        ]

    else
        []


type alias RadialProgress =
    { radius : Int
    , progress : Float
    , stroke : Int
    , progressColour : Element.Color
    , incompleteColour : Element.Color
    , fillColour : Element.Color
    }


radialProgressUi : RadialProgress -> Element msg
radialProgressUi radialProgress =
    let
        innerRadius =
            radialProgress.radius - radialProgress.stroke

        innerCircumference =
            pi * toFloat innerRadius * 2
    in
    html <|
        Svg.svg
            [ Svg.Attributes.height <| String.fromInt (radialProgress.radius * 2)
            , Svg.Attributes.width <| String.fromInt (radialProgress.radius * 2)
            ]
            [ Svg.circle
                -- background fill
                [ Svg.Attributes.r <| String.fromInt innerRadius
                , Svg.Attributes.cx <| String.fromInt radialProgress.radius
                , Svg.Attributes.cy <| String.fromInt radialProgress.radius
                , Svg.Attributes.fill <| rgbToStyle <| Element.toRgb <| radialProgress.fillColour
                ]
                []
            , Svg.circle
                -- incomplete progress stroke
                [ Svg.Attributes.r <| String.fromInt innerRadius
                , Svg.Attributes.cx <| String.fromInt radialProgress.radius
                , Svg.Attributes.cy <| String.fromInt radialProgress.radius
                , Svg.Attributes.strokeWidth <| String.fromInt radialProgress.stroke
                , Svg.Attributes.strokeDasharray <| String.fromFloat innerCircumference ++ " " ++ String.fromFloat innerCircumference
                , Svg.Attributes.strokeDashoffset <| String.fromFloat <| radialProgress.progress * innerCircumference
                , Svg.Attributes.style <| "transform: rotate(" ++ (String.fromFloat <| (radialProgress.progress * 360) - 90) ++ "deg); transform-origin: 50% 50%;"
                , Svg.Attributes.stroke <| rgbToStyle <| Element.toRgb <| radialProgress.incompleteColour
                , Svg.Attributes.fill "transparent"
                ]
                []
            , Svg.circle
                -- progress stroke
                [ Svg.Attributes.r <| String.fromInt innerRadius
                , Svg.Attributes.cx <| String.fromInt radialProgress.radius
                , Svg.Attributes.cy <| String.fromInt radialProgress.radius
                , Svg.Attributes.strokeWidth <| String.fromInt radialProgress.stroke
                , Svg.Attributes.strokeDasharray <| String.fromFloat innerCircumference ++ " " ++ String.fromFloat innerCircumference
                , Svg.Attributes.strokeDashoffset <| String.fromFloat <| (1 - radialProgress.progress) * innerCircumference
                , Svg.Attributes.style "transform: rotate(-90deg); transform-origin: 50% 50%;"
                , Svg.Attributes.stroke <| rgbToStyle <| Element.toRgb <| radialProgress.progressColour
                , Svg.Attributes.fill "transparent"
                ]
                []
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

        ( { holes, players, round, inTurn, button }, seed3 ) =
            Random.step
                (Random.map5
                    (\h ps r b btn ->
                        { holes = h
                        , players = ps
                        , round = r
                        , inTurn = b
                        , button = btn
                        }
                    )
                    holesGen
                    (Random.list 5 playerGen)
                    roundGen
                    Random.Extra.bool
                    (Random.int 0 5)
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
                button
                Nothing
                (Just (getPlayer 0).playerId)
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
