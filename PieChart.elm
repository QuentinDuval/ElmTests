module PieChart
    exposing
        ( PieSlice
        , PieArea
        , zooPieChart
        )

import Svg exposing (Svg, svg, g, path, text_, text)
import Svg.Attributes as Svg exposing (d, style, textAnchor)
import SvgUtils exposing (..)
import Visualization.Shape as Shape exposing (defaultPieConfig)


{-
   Creates a simple pie chart, declaratively, given:
   * The overall shape of the pie via `PieArea`
   * The information about each pie in `PieSlice`
-}


type alias PieArea =
    { outerRadius : Float
    , innerRadius : Float
    }


type alias PieSlice msg =
    { name : String
    , value : Int
    , fillColor : String
    , fontAttributes : List (Svg.Attribute msg)
    }


zooPieChart : PieArea -> List (PieSlice msg) -> Svg msg
zooPieChart { outerRadius, innerRadius } rawData =
    let
        pieCenter =
            ( outerRadius, outerRadius )

        model =
            List.filter (\slice -> slice.value /= 0) rawData

        slices =
            model
                |> Shape.pie
                    { defaultPieConfig
                        | outerRadius = outerRadius
                        , innerRadius = innerRadius
                        , sortingFn = (\a b -> compare a.name b.name)
                        , valueFn = (.value >> toFloat)
                    }

        svgSlice slice { fillColor } =
            path
                [ d (Shape.arc slice)
                , style ("fill:" ++ fillColor ++ "; stroke: #fff;")
                ]
                []

        svgLabel slice { name, fontAttributes } =
            let
                sliceCentroid =
                    Shape.centroid slice
            in
                text_
                    (translate sliceCentroid :: textAnchor "middle" :: fontAttributes)
                    [ text name ]
    in
        g [ translate pieCenter ]
            [ g [] (List.map2 svgSlice slices model)
            , g [] (List.map2 svgLabel slices model)
            ]



--
