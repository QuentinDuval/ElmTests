module ZooGraph
    exposing
        ( PieSlice
        , PieArea
        , zooPieChart
        )

import Visualization.Shape as Shape exposing (defaultPieConfig)
import Svg exposing (Svg, svg, g, path, text_, text)
import Svg.Attributes exposing (transform, d, style, dy, width, height, textAnchor)


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
zooPieChart { pieWidth, pieHeight } rawModel =
    let
        model =
            List.filter (\slice -> slice.value /= 0) rawModel

        radius =
            min pieWidth pieHeight / 2

        pieData =
            model
                |> List.map .value
                |> List.map toFloat
                |> Shape.pie
                    { defaultPieConfig
                        | outerRadius = radius
                        , innerRadius = radius / 2
                    }

        svgSlice pieDatum { fillColor } =
            path
                [ d (Shape.arc pieDatum)
                , style ("fill:" ++ fillColor ++ "; stroke: #fff;")
                ]
                []

        svgLabel slice { name } =
            let
                sliceCentroid =
                    Shape.centroid slice
            in
                text_
                    [ transform ("translate" ++ toString sliceCentroid), dy "0.35em", textAnchor "middle" ]
                    [ text name ]

        pieCenter =
            ( pieWidth / 2, pieHeight / 2 )
    in
        g [ transform ("translate " ++ toString pieCenter) ]
            [ g [] (List.map2 svgSlice pieData model)
            , g [] (List.map2 svgLabel pieData model)
            ]



--
