module Views.UI exposing (..)

import Browser
import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import FontAwesome.Attributes as Icon
import FontAwesome.Icon as Icon exposing (Icon)
import FontAwesome.Solid as Icon
import FontAwesome.Styles
import Model exposing (Model, Msg)


view : Model -> Browser.Document Msg
view model =
    { title = "pokerdot"
    , body =
        [ layout
            [ Font.family
                [ Font.typeface "Nunito"
                , Font.sansSerif
                ]
            , Background.color <| rgb255 191 189 193
            ]
          <|
            Element.column
                [ height fill
                , width fill
                ]
                [ -- header
                  Element.row
                    []
                    [ text "header" ]

                -- body
                , Element.row
                    []
                    [ text "body" ]

                -- footer
                , Element.row
                    []
                    [ text "footer" ]
                ]
        ]
    }
