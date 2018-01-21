module StackedBars exposing (stackBars, Series)

import Dict exposing (Dict)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Visualization.Axis as Axis exposing (defaultOptions)
import Visualization.Shape as Shape exposing (StackConfig, StackResult)
import Visualization.Scale as Scale exposing (BandConfig, BandScale, ContinuousScale, defaultBandConfig)
import List.Extra as List


--------------------------------------------------------------------------------
-- Input data to stack
--------------------------------------------------------------------------------
-- TODO : add legend


type alias Series a k =
    { key : a -> k
    , values :
        List
            { label : String
            , accessor : a -> Int
            , fillColor : String
            }
    }


largestAtBottom : List ( k, List number ) -> List ( k, List number )
largestAtBottom =
    List.sortBy (Tuple.second >> List.sum >> negate)


stackData : List a -> Series a k -> StackConfig String
stackData inputData series =
    { data = samples inputData series
    , offset = Shape.stackOffsetNone
    , order = largestAtBottom
    }


samples : List a -> Series a k -> List ( String, List Float )
samples inputData { values } =
    List.map
        (\{ label, accessor } -> ( label, List.map (accessor >> toFloat) inputData ))
        values



--------------------------------------------------------------------------------
-- Create a color for the provided year
--------------------------------------------------------------------------------


type alias ColumnBlock =
    { label : String
    , upperY : Float
    , lowerY : Float
    }


column : BandScale key -> Dict String String -> ( key, List ColumnBlock ) -> Svg msg
column xScale colorsByLabel ( key, taggedValues ) =
    let
        makeBlock { label, upperY, lowerY } =
            rect
                [ x <| toString <| Scale.convert xScale key
                , y <| toString <| lowerY
                , width <| toString <| Scale.bandwidth xScale
                , height <| toString <| (abs <| upperY - lowerY)
                , fill (Dict.get label colorsByLabel |> Maybe.withDefault "#FFFFFF")
                ]
                []
    in
        g [] (List.map makeBlock taggedValues)



--------------------------------------------------------------------------------
-- Assemble the columns together with the axis
--------------------------------------------------------------------------------


type alias GraphArea =
    { width : Float, height : Float }


stackBars : GraphArea -> List a -> Series a k -> Svg msg
stackBars canvas inputData series =
    let
        { values, labels, extent } =
            Shape.stack (stackData inputData series)

        keys =
            List.map series.key inputData

        colors =
            Dict.fromList <|
                List.map (\input -> ( input.label, input.fillColor )) series.values

        padding =
            { top = 30
            , left = 60
            , right = 30
            , bottom = 60
            }

        xScale : BandScale k
        xScale =
            Scale.band
                { defaultBandConfig
                    | paddingInner = 0.1
                    , paddingOuter = 0.2
                }
                keys
                ( 0, canvas.width - (padding.top + padding.bottom) )

        yScale : ContinuousScale
        yScale =
            Scale.linear extent ( canvas.height - (padding.left + padding.right), 0 )
                |> flip Scale.nice 4

        axisOptions =
            Axis.defaultOptions

        xAxis : Svg msg
        xAxis =
            Scale.toRenderable xScale
                |> Axis.axis
                    { axisOptions
                        | orientation = Axis.Bottom
                        , tickCount = 10
                    }

        yAxis : Svg msg
        yAxis =
            yScale
                |> Axis.axis { axisOptions | orientation = Axis.Left }

        valuesByKey : List (List ( Float, Float ))
        valuesByKey =
            List.transpose values

        columnValues : List (List ColumnBlock)
        columnValues =
            List.map
                (\columnBlocks ->
                    List.map2
                        (\label ( y1, y2 ) ->
                            { upperY = Scale.convert yScale y1
                            , lowerY = Scale.convert yScale y2
                            , label = label
                            }
                        )
                        labels
                        columnBlocks
                )
                valuesByKey
    in
        svg []
            [ g [ translate ( padding.left - 1, canvas.height - padding.bottom ) ] [ xAxis ]
            , g [ translate ( padding.left - 1, padding.top ) ] [ yAxis ]
            , g [ translate ( padding.left, padding.top ) ] <|
                List.map
                    (column xScale colors)
                    (List.map2 (,) keys columnValues)
            ]


translate : ( number, number ) -> Svg.Attribute msg
translate ( x, y ) =
    transform ("translate " ++ toString ( x, y ))



--
