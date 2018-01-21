module ColorScheme exposing (..)

import Color.Convert exposing (colorToCssRgb)
import Visualization.Scale as Scale


colors : Int -> List String
colors size =
    let
        -- map values from [0..size - 1] to float values in [0..1]
        lengthScale =
            Scale.linear ( 0, toFloat size - 1 ) ( 0, 1 )

        -- transform a float in [0..1] to a color
        toColor progression =
            Scale.viridisInterpolator (1 - progression)
                |> colorToCssRgb
    in
        List.range 0 (size - 1)
            |> List.map (toFloat >> Scale.convert lengthScale >> toColor)
