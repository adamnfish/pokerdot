module Views.Elements exposing (dotContainer, pdButton, pdButtonSmall, pdTab, pdText, zWidths)

import Browser.Dom exposing (Viewport)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (labelAbove, labelLeft)
import Element.Region as Region
import Model exposing (Msg)


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
