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
import ZipList exposing (..)
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


type alias Label =
    String


type alias Color =
    String


type alias Projection a =
    { label : Label
    , accessor : a -> Int
    , fillColor : Color
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


stackData : List a -> Series a k -> StackConfig String
stackData inputData series =
    { data = samples inputData series
    , offset = Shape.stackOffsetNone
    , order = largestAtBottom
    }


samples : List a -> Series a k -> List ( Label, List Float )
samples inputData { projections } =
    List.map
        (\{ label, accessor } -> ( label, List.map (accessor >> toFloat) inputData ))
        projections


largestAtBottom : List ( k, List number ) -> List ( k, List number )
largestAtBottom =
    List.sortBy (Tuple.second >> List.sum >> negate)



--------------------------------------------------------------------------------


type alias StackElement =
    { label : Label
    , upperY : Float
    , lowerY : Float
    }


stackedColumn : BandScale key -> Dict Label Color -> key -> List StackElement -> Svg msg
stackedColumn xScale colorsByLabel key stackElements =
    let
        labelColor label =
            Dict.get label colorsByLabel |> Maybe.withDefault "#FFFFFF"

        makeBlock { label, upperY, lowerY } =
            rect
                [ x := Scale.convert xScale key
                , y := lowerY
                , width := Scale.bandwidth xScale
                , height := abs (upperY - lowerY)
                , fill (labelColor label)
                ]
                []
    in
        g [] (List.map makeBlock stackElements)



--------------------------------------------------------------------------------


stackedBars : Config -> List a -> Series a k -> Svg msg
stackedBars canvas inputData series =
    let
        { values, labels, extent } =
            Shape.stack (stackData inputData series)

        keys =
            List.map series.key inputData

        colorsByLabel =
            List.map (\input -> ( input.label, input.fillColor )) series.projections
                |> Dict.fromList

        padding =
            { top = 30, left = 60, right = 30, bottom = 60 }

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
                |> Axis.axis { axisOptions | orientation = Axis.Bottom, tickCount = 10 }

        yAxis : Svg msg
        yAxis =
            yScale
                |> Axis.axis { axisOptions | orientation = Axis.Left }

        stackElements : List (List StackElement)
        stackElements =
            let
                valuesByKey =
                    List.transpose values

                stackElement label ( y1, y2 ) =
                    { upperY = Scale.convert yScale y1
                    , lowerY = Scale.convert yScale y2
                    , label = label
                    }
            in
                List.map
                    (\columnBlocks -> stackElement <$> labels <*> columnBlocks)
                    valuesByKey
    in
        svg []
            [ g [ translate ( padding.left - 1, canvas.height - padding.bottom ) ] [ xAxis ]
            , g [ translate ( padding.left - 1, padding.top ) ] [ yAxis ]
            , g [ translate ( padding.left, padding.top ) ] <|
                stackedColumn xScale colorsByLabel
                    <$> keys
                    <*> stackElements
            ]



--------------------------------------------------------------------------------
-- PRIVATE: Construction of a side by side bar chart
--------------------------------------------------------------------------------


sideBySideColumn : BandScale key -> ( key, Float ) -> Svg msg
sideBySideColumn xScale ( key, value ) =
    g []
        [ rect
            [ x := Scale.convert xScale key
            , y := value
            , width := Scale.bandwidth xScale
            , height := value
            ]
            []
        , text_
            [ x := Scale.convert (Scale.toRenderable xScale) key
            , y := (value - 5)
            , textAnchor "middle"
            ]
            [ text := value ]
        ]


sideBySideBars : Config -> List a -> Series a k -> Svg msg
sideBySideBars canvas inputData series =
    let
        padding =
            { top = 30, left = 60, right = 30, bottom = 60 }

        keys =
            List.map series.key inputData

        valuesByLabel : List ( Label, List Float )
        valuesByLabel =
            samples inputData series

        allValues =
            List.concatMap Tuple.second valuesByLabel

        extent =
            ( 0, List.maximum allValues |> Maybe.withDefault 0 )

        xScale : BandScale k
        xScale =
            Scale.band
                { defaultBandConfig | paddingInner = 0.1, paddingOuter = 0.2 }
                keys
                ( 0, canvas.width - (padding.top + padding.bottom) )

        yScale : ContinuousScale
        yScale =
            Scale.linear extent ( canvas.height - (padding.left + padding.right), 0 )
                |> flip Scale.nice 4

        xAxis : Svg msg
        xAxis =
            Scale.toRenderable xScale
                |> Axis.axis { defaultOptions | orientation = Axis.Bottom }

        yAxis : Svg msg
        yAxis =
            yScale
                |> Axis.axis { defaultOptions | orientation = Axis.Left, tickCount = 5 }

        fakeValuesTO_REPLACE =
            valuesByLabel
                |> List.head
                |> Maybe.map (Tuple.second >> (\vals -> (,) <$> keys <*> vals))
                |> Maybe.withDefault []
    in
        svg []
            [ g [ translate ( padding.left - 1, canvas.height - padding.bottom ) ] [ xAxis ]
            , g [ translate ( padding.left - 1, padding.top ) ] [ yAxis ]
            , g [ translate ( padding.left, padding.top ) ] <|
                List.map
                    (sideBySideColumn xScale)
                    fakeValuesTO_REPLACE
            ]



--
