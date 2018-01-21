module Elementary exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Dict exposing (Dict)
import Svg exposing (svg)
import Svg.Attributes exposing (width, height, viewBox)
import PieChart exposing (..)
import StackedBars exposing (..)


main : Program Never Zoo ZooMsg
main =
    Html.program
        { init = initZoo
        , view = viewZoo
        , update = updateZoo
        , subscriptions = \_ -> Sub.none
        }


type alias Zoo =
    { population : Dict String Int
    }


type ZooMsg
    = AddBeast String
    | ResetZoo


initZoo : ( Zoo, Cmd ZooMsg )
initZoo =
    ( emptyZoo, Cmd.none )


species : List String
species =
    [ "Elephant", "Sea Lion", "Parakeet" ]


speciesCount : Zoo -> String -> Int
speciesCount zoo name =
    Maybe.withDefault 0 (Dict.get name zoo.population)


emptyZoo : Zoo
emptyZoo =
    { population = Dict.fromList (List.map (\b -> ( b, 1 )) species) }


type alias Year =
    Int


type alias YearRecord =
    { year : Year
    , elephant : Int
    , seaLion : Int
    , parakeet : Int
    }


viewZoo : Zoo -> Html ZooMsg
viewZoo zoo =
    let
        population =
            List.intersperse (br [] []) (viewPopulation zoo)

        yearRecords =
            [ YearRecord 2004 16148 95089 401470
            , YearRecord 2005 16740 94347 417438
            , YearRecord 2006 17309 94472 449246
            , YearRecord 2007 17128 92160 447324
            , YearRecord 2008 16465 90750 443563
            , YearRecord 2009 15399 89241 408742
            , YearRecord 2010 14722 85593 369089
            , YearRecord 2011 14661 84175 354746
            , YearRecord 2012 14856 85141 355051
            , YearRecord 2013 14196 79770 345031
            ]

        series : Series YearRecord Int
        series =
            { key = .year
            , values =
                [ { label = "Elephant", accessor = .elephant }
                , { label = "Sea Lion", accessor = .seaLion }
                , { label = "Parakeet", accessor = .parakeet }
                ]
            }
    in
        div []
            (h4 [] [ text "Zoo population" ]
                :: population
                ++ [ br [] []
                   , button [ onClick ResetZoo ] [ text "Reset zoo" ]
                   , br [] []
                   , svg
                        [ width "400", height "400", viewBox "0 0 400 400" ]
                        [ zooPieChart { pieWidth = 400, pieHeight = 400 } (populationPieSlices zoo) ]

                   -- TODO: sample the population with a button, and display the evoluation over time
                   , svg
                        [ width "800", height "400", viewBox "0 0 800 400" ]
                        [ stackBars { width = 800, height = 400 } yearRecords series ]
                   ]
            )


populationPieSlices : Zoo -> List PieSlice
populationPieSlices { population } =
    let
        baseColors =
            [ "#98abc5"
            , "#8a89a6"
            , "#7b6888"
            , "#6b486b"
            , "#a05d56"
            , "#d0743c"
            , "#ff8c00"
            ]

        colors =
            List.concat <|
                List.repeat
                    (1 + Dict.size population // List.length baseColors)
                    baseColors
    in
        List.map2
            (\( beast, nb ) c -> { name = beast, value = nb, fillColor = c })
            (Dict.toList population)
            colors


viewPopulation : Zoo -> List (Html ZooMsg)
viewPopulation zoo =
    List.map
        (\beast ->
            div []
                [ text (beast ++ ": " ++ toString (speciesCount zoo beast) ++ " ")
                , button [ onClick (AddBeast beast) ] [ text ("Add " ++ beast) ]
                ]
        )
        species


updateZoo : ZooMsg -> Zoo -> ( Zoo, Cmd ZooMsg )
updateZoo msg zoo =
    case msg of
        AddBeast name ->
            ( onAddBeast zoo name, Cmd.none )

        ResetZoo ->
            ( emptyZoo, Cmd.none )


onAddBeast : Zoo -> String -> Zoo
onAddBeast zoo name =
    let
        addOne nb =
            Just (1 + Maybe.withDefault 0 nb)
    in
        { zoo | population = Dict.update name addOne zoo.population }



--
