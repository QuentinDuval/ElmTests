module StackedBars exposing (renderSvg)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Color.Convert exposing (colorToCssRgb)
import Visualization.Axis as Axis exposing (defaultOptions)
import Visualization.Shape as Shape exposing (StackConfig, StackResult)
import Visualization.Scale as Scale exposing (BandConfig, BandScale, ContinuousScale, defaultBandConfig)
import Color exposing (Color)
import List.Extra as List


renderSvg : Svg msg
renderSvg =
    view (Shape.stack config)


type alias Year =
    Int


type alias CrimeRate =
    { year : Int
    , population : Int
    , murder : Int
    , rape : Int
    , robbery : Int
    , assault : Int
    , burglary : Int
    , larceny : Int
    , motorTheft : Int
    }


type alias Series =
    List { label : String, accessor : CrimeRate -> Int }


series : Series
series =
    [ { label = "Assault", accessor = .assault }
    , { label = "Rape", accessor = .rape }
    , { label = "Robbery", accessor = .robbery }
    , { label = "Murder", accessor = .murder }
    ]


crimeRates : List CrimeRate
crimeRates =
    [ CrimeRate 1994 260327021 23326 102216 618949 1113179 2712774 7879812 1539287
    , CrimeRate 1995 262803276 21606 97470 580509 1099207 2593784 7997710 1472441
    , CrimeRate 1996 265228572 19645 96252 535594 1037049 2506400 7904685 1394238
    , CrimeRate 1997 267783607 18208 96153 498534 1023201 2460526 7743760 1354189
    , CrimeRate 1998 270248003 16974 93144 447186 976583 2332735 7376311 1242781
    , CrimeRate 1999 272690813 15522 89411 409371 911740 2100739 6955520 1152075
    , CrimeRate 2000 281421906 15586 90178 408016 911706 2050992 6971590 1160002
    , CrimeRate 2001 285317559 16037 90863 423557 909023 2116531 7092267 1228391
    , CrimeRate 2002 287973924 16229 95235 420806 891407 2151252 7057379 1246646
    , CrimeRate 2003 290788976 16528 93883 414235 859030 2154834 7026802 1261226
    , CrimeRate 2004 293656842 16148 95089 401470 847381 2144446 6937089 1237851
    , CrimeRate 2005 296507061 16740 94347 417438 862220 2155448 6783447 1235859
    , CrimeRate 2006 299398484 17309 94472 449246 874096 2194993 6626363 1198245
    , CrimeRate 2007 301621157 17128 92160 447324 866358 2190198 6591542 1100472
    , CrimeRate 2008 304059724 16465 90750 443563 843683 2228887 6586206 959059
    , CrimeRate 2009 307006550 15399 89241 408742 812514 2203313 6338095 795652
    , CrimeRate 2010 309330219 14722 85593 369089 781844 2168459 6204601 739565
    , CrimeRate 2011 311587816 14661 84175 354746 752423 2185140 6151095 716508
    , CrimeRate 2012 313873685 14856 85141 355051 762009 2109932 6168874 723186
    , CrimeRate 2013 316128839 14196 79770 345031 724149 1928465 6004453 699594
    ]


samples : Series -> List ( String, List Float )
samples =
    List.map
        (\{ label, accessor } -> ( label, List.map (toFloat << accessor) crimeRates ))


canvas : { width : Float, height : Float }
canvas =
    { width = 800
    , height = 400
    }


padding : { bottom : number, left : number1, right : number2, top : number3 }
padding =
    { top = 30
    , left = 60
    , right = 30
    , bottom = 60
    }


config : StackConfig String
config =
    { data = samples series
    , offset = Shape.stackOffsetNone
    , order =
        -- stylistic choice: largest (by sum of values) category at the bottom
        List.sortBy (Tuple.second >> List.sum >> negate)
    }


colors : Int -> List String
colors size =
    let
        -- map values from [0..size - 1] to float values in [0..1]
        lengthScale =
            Scale.linear ( 0, toFloat size - 1 ) ( 0, 1 )

        toColor progression =
            Scale.viridisInterpolator (1 - progression)
                |> colorToCssRgb
    in
        List.range 0 (size - 1)
            |> List.map (toFloat >> Scale.convert lengthScale >> toColor)


column : BandScale Year -> ( Year, List ( Float, Float ) ) -> Svg msg
column xScale ( year, values ) =
    let
        block color ( upperY, lowerY ) =
            rect
                [ x <| toString <| Scale.convert xScale year
                , y <| toString <| lowerY
                , width <| toString <| Scale.bandwidth xScale
                , height <| toString <| (abs <| upperY - lowerY)
                , fill color
                ]
                []
    in
        g [] (List.map2 block (colors (List.length values)) values)


view : StackResult String -> Svg msg
view { values, labels, extent } =
    let
        -- transpose back to get the values per year
        yearValues =
            List.transpose values

        years =
            List.map .year crimeRates

        xScale : BandScale Year
        xScale =
            Scale.band { defaultBandConfig | paddingInner = 0.1, paddingOuter = 0.2 } years ( 0, canvas.width - (padding.top + padding.bottom) )

        yScale : ContinuousScale
        yScale =
            Scale.linear extent ( canvas.height - (padding.left + padding.right), 0 )
                |> flip Scale.nice 4

        axisOptions =
            Axis.defaultOptions

        xAxis : Svg msg
        xAxis =
            Axis.axis { axisOptions | orientation = Axis.Bottom, tickCount = 10 } (Scale.toRenderable xScale)

        yAxis : Svg msg
        yAxis =
            Axis.axis { axisOptions | orientation = Axis.Left } yScale

        scaledValues =
            List.map (List.map (\( y1, y2 ) -> ( Scale.convert yScale y1, Scale.convert yScale y2 ))) yearValues
    in
        svg [ width (toString canvas.width ++ "px"), height (toString canvas.height ++ "px") ]
            [ g [ translate (padding.left - 1) (canvas.height - padding.bottom) ]
                [ xAxis ]
            , g [ translate (padding.left - 1) padding.top ]
                [ yAxis ]
            , g [ translate padding.left padding.top, class "series" ] <|
                List.map (column xScale) (List.map2 (,) years scaledValues)
            ]


translate : number -> number -> Svg.Attribute msg
translate x y =
    transform ("translate " ++ toString ( x, y ))



--
