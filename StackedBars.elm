module StackedBars exposing (renderSvg)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Color.Convert exposing (colorToCssRgb)
import Visualization.Axis as Axis exposing (defaultOptions)
import Visualization.Shape as Shape exposing (StackConfig, StackResult)
import Visualization.Scale as Scale exposing (BandConfig, BandScale, ContinuousScale, defaultBandConfig)
import List.Extra as List


--------------------------------------------------------------------------------
-- Input data
--------------------------------------------------------------------------------


type alias Year =
    Int


type alias CrimeRate =
    { year : Year
    , population : Int
    , murder : Int
    , rape : Int
    , robbery : Int
    , assault : Int
    }


crimeRates : List CrimeRate
crimeRates =
    [ CrimeRate 1994 260327021 23326 102216 618949 1113179
    , CrimeRate 1995 262803276 21606 97470 580509 1099207
    , CrimeRate 1996 265228572 19645 96252 535594 1037049
    , CrimeRate 1997 267783607 18208 96153 498534 1023201
    , CrimeRate 1998 270248003 16974 93144 447186 976583
    , CrimeRate 1999 272690813 15522 89411 409371 911740
    , CrimeRate 2000 281421906 15586 90178 408016 911706
    , CrimeRate 2001 285317559 16037 90863 423557 909023
    , CrimeRate 2002 287973924 16229 95235 420806 891407
    , CrimeRate 2003 290788976 16528 93883 414235 859030
    , CrimeRate 2004 293656842 16148 95089 401470 847381
    , CrimeRate 2005 296507061 16740 94347 417438 862220
    , CrimeRate 2006 299398484 17309 94472 449246 874096
    , CrimeRate 2007 301621157 17128 92160 447324 866358
    , CrimeRate 2008 304059724 16465 90750 443563 843683
    , CrimeRate 2009 307006550 15399 89241 408742 812514
    , CrimeRate 2010 309330219 14722 85593 369089 781844
    , CrimeRate 2011 311587816 14661 84175 354746 752423
    , CrimeRate 2012 313873685 14856 85141 355051 762009
    , CrimeRate 2013 316128839 14196 79770 345031 724149
    ]



--------------------------------------------------------------------------------
-- Input data to stack
--------------------------------------------------------------------------------


stackData : StackConfig String
stackData =
    { data = samples series
    , offset = Shape.stackOffsetNone

    -- stylistic choice: largest (by sum of values) category at the bottom
    , order = List.sortBy (Tuple.second >> List.sum >> negate)
    }


samples : Series CrimeRate -> List ( String, List Float )
samples =
    List.map
        (\{ label, accessor } -> ( label, List.map (toFloat << accessor) crimeRates ))


type alias Series input =
    List { label : String, accessor : input -> Int }


series : Series CrimeRate
series =
    [ { label = "Assault", accessor = .assault }
    , { label = "Rape", accessor = .rape }
    , { label = "Robbery", accessor = .robbery }
    , { label = "Murder", accessor = .murder }
    ]



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


column : BandScale Year -> ( Year, List ( Float, Float ) ) -> Svg msg
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


view : GraphArea -> StackResult String -> Svg msg
view canvas { values, labels, extent } =
    let
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
            List.map .year crimeRates

        xScale : BandScale Year
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


renderSvg : GraphArea -> Svg msg
renderSvg area =
    view area (Shape.stack stackData)


translate : ( number, number ) -> Svg.Attribute msg
translate ( x, y ) =
    transform ("translate " ++ toString ( x, y ))



--
