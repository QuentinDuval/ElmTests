module SvgUtils exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)


translate : ( number, number ) -> Svg.Attribute msg
translate ( x, y ) =
    transform ("translate " ++ toString ( x, y ))
