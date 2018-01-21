module PieChart
    exposing
        ( PieSlice
        , PieArea
        , zooPieChart
        )

import Visualization.Shape as Shape exposing (defaultPieConfig)
import Svg exposing (Svg, svg, g, path, text_, text)
import Svg.Attributes exposing (transform, d, style, dy, width, height, textAnchor)
import SvgUtils exposing (..)


type alias PieArea =
    { pieWidth : Float
    , pieHeight : Float
    }


type alias PieSlice =
    { name : String
    , value : Int
    , fillColor : String
    }


zooPieChart : PieArea -> List PieSlice -> Svg msg
zooPieChart { pieWidth, pieHeight } rawData =
    let
        model =
            List.filter (\slice -> slice.value /= 0) rawData

        radius =
            min pieWidth pieHeight / 2

        slices =
            model
                |> Shape.pie
                    { defaultPieConfig
                        | outerRadius = radius
                        , innerRadius = radius / 2
                        , sortingFn = (\a b -> compare a.name b.name)
                        , valueFn = (.value >> toFloat)
                    }

        svgSlice slice { fillColor } =
            path
                [ d (Shape.arc slice)
                , style ("fill:" ++ fillColor ++ "; stroke: #fff;")
                ]
                []

        svgLabel slice { name } =
            let
                sliceCentroid =
                    Shape.centroid slice
            in
                text_
                    [ translate sliceCentroid, textAnchor "middle" ]
                    [ text name ]

        pieCenter =
            ( pieWidth / 2, pieHeight / 2 )
    in
        g [ translate pieCenter ]
            [ g [] (List.map2 svgSlice slices model)
            , g [] (List.map2 svgLabel slices model)
            ]



--
