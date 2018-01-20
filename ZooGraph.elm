module ZooGraph
    exposing
        ( PieSlice
        , PieArea
        , zooPieChart
        )

import Visualization.Shape as Shape exposing (defaultPieConfig)
import Array exposing (Array)
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
                |> Shape.pie { defaultPieConfig | outerRadius = radius }

        makeSlice pieDatum { fillColor } =
            path
                [ d (Shape.arc pieDatum)
                , style ("fill:" ++ fillColor ++ "; stroke: #fff;")
                ]
                []

        makeLabel slice { name } =
            text_
                [ transform ("translate" ++ toString (Shape.centroid { slice | innerRadius = radius - 40, outerRadius = radius - 40 }))
                , dy ".35em"
                , textAnchor "middle"
                ]
                [ text name ]
    in
        svg [ width (toString pieWidth ++ "px"), height (toString pieHeight ++ "px") ]
            [ g [ transform ("translate(" ++ toString (pieWidth / 2) ++ "," ++ toString (pieHeight / 2) ++ ")") ]
                [ g [] <| List.map2 makeSlice pieData model
                , g [] <| List.map2 makeLabel pieData model
                ]
            ]



--
