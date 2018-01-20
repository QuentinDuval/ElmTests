module ZooGraph exposing (zooPieChart)

import Visualization.Shape as Shape exposing (defaultPieConfig)
import Array exposing (Array)
import Svg exposing (Svg, svg, g, path, text_, text)
import Svg.Attributes exposing (transform, d, style, dy, width, height, textAnchor)


type alias PieArea =
    { pieWidth : Float
    , pieHeight : Float
    }


zooPieChart : PieArea -> List ( String, Int ) -> Svg msg
zooPieChart { pieWidth, pieHeight } rawModel =
    let
        model =
            List.filter (\( _, v ) -> v /= 0) rawModel

        radius =
            min pieWidth pieHeight / 2

        pieData =
            model
                |> List.map Tuple.second
                |> List.map toFloat
                |> Shape.pie { defaultPieConfig | outerRadius = radius }

        makeSlice index datum =
            path
                [ d (Shape.arc datum)
                , style ("fill:" ++ (Maybe.withDefault "#000" <| Array.get index colors) ++ "; stroke: #fff;")
                ]
                []

        makeLabel slice ( label, value ) =
            text_
                [ transform ("translate" ++ toString (Shape.centroid { slice | innerRadius = radius - 40, outerRadius = radius - 40 }))
                , dy ".35em"
                , textAnchor "middle"
                ]
                [ text label ]
    in
        svg [ width (toString pieWidth ++ "px"), height (toString pieHeight ++ "px") ]
            [ g [ transform ("translate(" ++ toString (pieWidth / 2) ++ "," ++ toString (pieHeight / 2) ++ ")") ]
                [ g [] <| List.indexedMap makeSlice pieData
                , g [] <| List.map2 makeLabel pieData model
                ]
            ]



-- Could be provided by client


colors : Array String
colors =
    Array.fromList
        [ "#98abc5"
        , "#8a89a6"
        , "#7b6888"
        , "#6b486b"
        , "#a05d56"
        , "#d0743c"
        , "#ff8c00"
        ]



{-
   model : List ( String, Float )
   model =
       [ ( "/notifications", 2704659 )
       , ( "/about", 4499890 )
       , ( "/product", 2159981 )
       , ( "/blog", 3853788 )
       , ( "/shop", 14106543 )
       , ( "/profile", 8819342 )
       , ( "/", 612463 )
       ]
-}
--
