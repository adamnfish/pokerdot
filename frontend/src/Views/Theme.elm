module Views.Theme exposing (Scheme, colours, dim, focusColour, glow, pressColour, scheme1, scheme2, scheme3, scheme4, scheme5, textColour)

import Element exposing (rgb255)


type alias Scheme =
    { main : Element.Color
    , highlight : Element.Color
    , text : Element.Color
    }


colours =
    { white =
        rgb255 255 255 255
    , black =
        rgb255 0 0 0
    , primary =
        rgb255 253 235 23
    , highlightPrimary =
        rgb255 87 197 224
    , highlightSecondary =
        rgb255 73 197 182
    , lowlight =
        rgb255 94 1 188
    , error =
        rgb255 233 79 55
    , icon =
        rgb255 67 250 0
    , disabled =
        rgb255 244 243 240
    }


textColour : Element.Color -> Element.Color
textColour colour =
    let
        rgba =
            Element.toRgb colour
    in
    Element.fromRgb { rgba | alpha = 0.9 }


scheme1 : Scheme
scheme1 =
    { main = rgb255 253 235 23
    , highlight = rgb255 0 151 189
    , text = rgb255 255 255 255
    }


scheme2 : Scheme
scheme2 =
    { main = rgb255 121 106 220
    , highlight = rgb255 67 250 0
    , text = rgb255 255 255 255
    }


scheme3 : Scheme
scheme3 =
    { main = rgb255 251 94 9
    , highlight = rgb255 94 1 188
    , text = rgb255 255 255 255
    }


scheme4 : Scheme
scheme4 =
    { main = rgb255 0 151 189
    , highlight = rgb255 255 247 77
    , text = rgb255 255 255 255
    }


scheme5 : Scheme
scheme5 =
    { main = rgb255 67 250 0
    , highlight = rgb255 251 94 9
    , text = rgb255 255 255 255
    }



-- Colour utilities


focusColour : Element.Color -> Element.Color
focusColour =
    darken 0.3


pressColour : Element.Color -> Element.Color
pressColour =
    lighten 0.2


glow : Element.Color -> Element.Color
glow =
    lighten 0.4


dim : Element.Color -> Element.Color
dim =
    darken 0.4


clamp : Float -> Float
clamp f =
    Basics.max 0 <| Basics.min 1 f


darken : Float -> Element.Color -> Element.Color
darken darkenFactor colour =
    let
        rgba =
            Element.toRgb colour
    in
    Element.fromRgb
        { red = clamp <| rgba.red * (1 - darkenFactor)
        , green = clamp <| rgba.green * (1 - darkenFactor)
        , blue = clamp <| rgba.blue * (1 - darkenFactor)
        , alpha = rgba.alpha
        }


lighten : Float -> Element.Color -> Element.Color
lighten lightenFactor colour =
    let
        rgba =
            Element.toRgb colour
    in
    Element.fromRgb
        { red = clamp <| ((1 - rgba.red) * lightenFactor) + rgba.red
        , green = clamp <| ((1 - rgba.green) * lightenFactor) + rgba.green
        , blue = clamp <| ((1 - rgba.blue) * lightenFactor) + rgba.blue
        , alpha = rgba.alpha
        }
