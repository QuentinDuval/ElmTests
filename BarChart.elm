module BarChart
    exposing
        ( barChart
        , Series
        , Positioning(..)
        )

import Dict exposing (Dict)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Visualization.Axis as Axis exposing (defaultOptions)
import Visualization.Shape as Shape exposing (StackConfig, StackResult)
import Visualization.Scale as Scale exposing (BandConfig, BandScale, ContinuousScale, defaultBandConfig)
import List.Extra as List
import SvgUtils exposing (..)


--------------------------------------------------------------------------------
-- Parameterization of the bar chart
--------------------------------------------------------------------------------
--
-- TODO : add legend (and configuration to decide whether you want one)
-- TODO : parameter to decide whether the bars are stacked or side by side
-- TODO : add possiblity to parameterize the font size
-- TODO : use relative sizing of the bars and such
--


type Positioning
    = SideBySide
    | Stacked


type alias Config =
    { width : Float
    , height : Float
    , positioning : Positioning
    }


type alias Projection a =
    { label : String
    , accessor : a -> Int
    , fillColor : String
    }


type alias Series a k =
    { key : a -> k
    , projections : List (Projection a)
    }


barChart : Config -> List a -> Series a k -> Svg msg
barChart config inputData series =
    case config.positioning of
        Stacked ->
            stackedBars config inputData series

        SideBySide ->
            sideBySideBars config inputData series



--------------------------------------------------------------------------------
-- PRIVATE: Construction of a stack (and the column of the stack)
--------------------------------------------------------------------------------


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
samples inputData { projections } =
    List.map
        (\{ label, accessor } -> ( label, List.map (accessor >> toFloat) inputData ))
        projections


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
                [ x := Scale.convert xScale key
                , y := lowerY
                , width := Scale.bandwidth xScale
                , height := abs (upperY - lowerY)
                , fill (Dict.get label colorsByLabel |> Maybe.withDefault "#FFFFFF")
                ]
                []
    in
        g [] (List.map makeBlock taggedValues)


stackedBars : Config -> List a -> Series a k -> Svg msg
stackedBars canvas inputData series =
    let
        { values, labels, extent } =
            Shape.stack (stackData inputData series)

        keys =
            List.map series.key inputData

        colors =
            Dict.fromList <|
                List.map (\input -> ( input.label, input.fillColor )) series.projections

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



--------------------------------------------------------------------------------
-- Side by side bars
--------------------------------------------------------------------------------


sideBySideBars : Config -> List a -> Series a k -> Svg msg
sideBySideBars canvas inputData series =
    svg [] []



--
