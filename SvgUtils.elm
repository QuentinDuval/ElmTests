module SvgUtils exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)


(:=) : (String -> b) -> a -> b
(:=) field val =
    field <| toString val


translate : ( number, number ) -> Svg.Attribute msg
translate ( x, y ) =
    transform ("translate " ++ toString ( x, y ))
