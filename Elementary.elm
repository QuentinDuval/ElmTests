module Elementary exposing (..)

import BarChart exposing (..)
import Html exposing (..)
import Html.Events exposing (..)
import Dict exposing (Dict)
import Svg exposing (svg)
import Svg.Attributes as Svg exposing (width, height, viewBox)
import PieChart exposing (..)


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


type alias SpeciesName =
    String


type alias Population =
    Dict SpeciesName Int


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
    List.sort [ "Elephant", "Sea Lion", "Parakeet" ]


speciesColors : List String
speciesColors =
    let
        -- TODO use the ColorScheme module
        baseColors =
            [ "#98abc5"
            , "#8a89a6"
            , "#7b6888"
            , "#6b486b"
            , "#a05d56"
            , "#d0743c"
            , "#ff8c00"
            ]
    in
        List.concat <|
            List.repeat
                (1 + List.length species // List.length baseColors)
                baseColors


getCount : SpeciesName -> Population -> Int
getCount beast =
    Dict.get beast >> Maybe.withDefault 0


reportProjection : Series PopulationByYear Int
reportProjection =
    { key = Tuple.first
    , projections =
        List.map2
            (\beast color -> { label = beast, accessor = Tuple.second >> getCount beast, fillColor = color })
            species
            speciesColors
    }


emptyZoo : Zoo
emptyZoo =
    let
        startPopulation =
            Dict.fromList (List.map (\b -> ( b, 10 )) species)
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
                [ zooPieChart { outerRadius = 200, innerRadius = 100 } (populationPieSlices zoo) ]
            , svg
                [ width "600", height "400", viewBox "0 0 600 400" ]
                -- [ barChart { width = 600, height = 400, positioning = Stacked } zoo.pastRecords reportProjection ]
                [ barChart { width = 600, height = 400, positioning = SideBySide } zoo.pastRecords reportProjection ]
            ]


populationPieSlices : Zoo -> List (PieSlice msg)
populationPieSlices { population } =
    List.map2
        (\( beast, nb ) c ->
            { name = beast
            , value = nb
            , fillColor = c
            , fontAttributes =
                [ Svg.fontSize "20px"
                , Svg.fontFamily "Verdata"
                , Svg.fill "#FFFFFF"
                ]
            }
        )
        (Dict.toList population)
        speciesColors


viewPopulation : Zoo -> List (Html ZooMsg)
viewPopulation zoo =
    List.map
        (\beast ->
            div []
                [ text (beast ++ ": " ++ toString (getCount beast zoo.population) ++ " ")
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
