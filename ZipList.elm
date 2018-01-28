module ZipList exposing (..)

import List.Extra as List


(<$>) : (a -> b) -> List a -> List b
(<$>) =
    List.map


(<*>) : List (a -> b) -> List a -> List b
(<*>) =
    flip List.andMap
