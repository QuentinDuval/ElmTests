module SvgUtils exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)


(:=) : (String -> Svg.Attribute msg) -> a -> Svg.Attribute msg
(:=) field val =
    field <| toString val


translate : ( number, number ) -> Svg.Attribute msg
translate ( x, y ) =
    transform ("translate " ++ toString ( x, y ))
