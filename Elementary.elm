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


type alias Year =
    Int


type alias Population =
    Dict String Int


type alias PopulationByYear =
    ( Year, Population )


type alias Zoo =
    { population : Population
    , currentYear : Year
    , pastRecords : List PopulationByYear
    }


type ZooMsg
    = AddBeast String (Int -> Int)
    | ResetZoo
    | AnnualReport


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
    let
        startPopulation =
            Dict.fromList (List.map (\b -> ( b, 1 )) species)
    in
        { population = startPopulation
        , currentYear = 2018
        , pastRecords = [ ( 2017, startPopulation ) ]
        }


viewZoo : Zoo -> Html ZooMsg
viewZoo zoo =
    let
        population =
            List.intersperse (br [] []) (viewPopulation zoo)

        series : Series PopulationByYear Int
        series =
            { key = Tuple.first
            , values =
                [ { label = "Elephant", accessor = Tuple.second >> Dict.get "Elephant" >> Maybe.withDefault 0 }
                , { label = "Sea Lion", accessor = Tuple.second >> Dict.get "Sea Lion" >> Maybe.withDefault 0 }
                , { label = "Parakeet", accessor = Tuple.second >> Dict.get "Parakeet" >> Maybe.withDefault 0 }
                ]
            }
    in
        div []
            [ h4 [] [ text "Zoo population" ]
            , div [] population
            , br [] []
            , button [ onClick ResetZoo ] [ text "Reset zoo" ]
            , button [ onClick AnnualReport ] [ text "Annual report" ]
            , br [] []
            , svg
                [ width "400", height "400", viewBox "0 0 400 400" ]
                [ zooPieChart { pieWidth = 400, pieHeight = 400 } (populationPieSlices zoo) ]
            , svg
                [ width "800", height "400", viewBox "0 0 800 400" ]
                [ stackBars { width = 800, height = 400 } zoo.pastRecords series ]
            ]


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
                , button [ onClick (AddBeast beast (\x -> x + 1)) ] [ text " + " ]
                , button [ onClick (AddBeast beast (\x -> x - 1)) ] [ text " - " ]
                ]
        )
        species


updateZoo : ZooMsg -> Zoo -> ( Zoo, Cmd ZooMsg )
updateZoo msg zoo =
    case msg of
        AddBeast name inc ->
            ( onAddBeast zoo name inc, Cmd.none )

        ResetZoo ->
            ( emptyZoo, Cmd.none )

        AnnualReport ->
            ( onAnnualReport zoo, Cmd.none )


onAddBeast : Zoo -> String -> (Int -> Int) -> Zoo
onAddBeast zoo name inc =
    let
        addOne nb =
            Just (inc <| Maybe.withDefault 0 nb)
    in
        { zoo | population = Dict.update name addOne zoo.population }


onAnnualReport : Zoo -> Zoo
onAnnualReport zoo =
    { zoo
        | currentYear = zoo.currentYear + 1
        , pastRecords = zoo.pastRecords ++ [ ( zoo.currentYear, zoo.population ) ]
    }



--
