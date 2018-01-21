module StackedBars exposing (stackBars, Series)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Color.Convert exposing (colorToCssRgb)
import Visualization.Axis as Axis exposing (defaultOptions)
import Visualization.Shape as Shape exposing (StackConfig, StackResult)
import Visualization.Scale as Scale exposing (BandConfig, BandScale, ContinuousScale, defaultBandConfig)
import List.Extra as List


--------------------------------------------------------------------------------
-- Input data to stack
--------------------------------------------------------------------------------


type alias Series a k =
    { key : a -> k
    , values : List { label : String, accessor : a -> Int }
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
-- A scheme to create a list of N colors
--------------------------------------------------------------------------------


colors : Int -> List String
colors size =
    let
        -- map values from [0..size - 1] to float values in [0..1]
        lengthScale =
            Scale.linear ( 0, toFloat size - 1 ) ( 0, 1 )

        -- transform a float in [0..1] to a color
        toColor progression =
            Scale.viridisInterpolator (1 - progression)
                |> colorToCssRgb
    in
        List.range 0 (size - 1)
            |> List.map (toFloat >> Scale.convert lengthScale >> toColor)



--------------------------------------------------------------------------------
-- Create a color for the provided year
--------------------------------------------------------------------------------


column : BandScale k -> ( k, List ( Float, Float ) ) -> Svg msg
column xScale ( year, values ) =
    let
        makeBlock fillColor ( upperY, lowerY ) =
            rect
                [ x <| toString <| Scale.convert xScale year
                , y <| toString <| lowerY
                , width <| toString <| Scale.bandwidth xScale
                , height <| toString <| (abs <| upperY - lowerY)
                , fill fillColor
                ]
                []

        blockColors =
            colors (List.length values)
    in
        g [] (List.map2 makeBlock blockColors values)



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

        padding =
            { top = 30
            , left = 60
            , right = 30
            , bottom = 60
            }

        -- transpose back to get the values per year
        yearValues =
            List.transpose values

        years =
            List.map series.key inputData

        xScale : BandScale k
        xScale =
            Scale.band
                { defaultBandConfig
                    | paddingInner = 0.1
                    , paddingOuter = 0.2
                }
                years
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

        scaledValues =
            yearValues
                |> (List.map >> List.map)
                    (\( y1, y2 ) -> ( Scale.convert yScale y1, Scale.convert yScale y2 ))
    in
        svg []
            [ g [ translate ( padding.left - 1, canvas.height - padding.bottom ) ] [ xAxis ]
            , g [ translate ( padding.left - 1, padding.top ) ] [ yAxis ]
            , g [ translate ( padding.left, padding.top ) ] <|
                List.map (column xScale) (List.map2 (,) years scaledValues)
            ]


translate : ( number, number ) -> Svg.Attribute msg
translate ( x, y ) =
    transform ("translate " ++ toString ( x, y ))



--
