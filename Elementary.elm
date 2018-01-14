module Elementary exposing (..)

import Html exposing (..)
import Html.Events exposing (..)


main : Program Never Zoo ZooMsg
main =
    Html.program
        { init = initZoo
        , view = viewZoo
        , update = updateZoo
        , subscriptions = \_ -> Sub.none
        }


type alias Zoo =
    { elephantCount : Int
    , seaLionCount : Int
    , parakeetCount : Int
    }


type ZooMsg
    = AddBeast (Zoo -> Zoo)
    | ResetZoo


initZoo : ( Zoo, Cmd ZooMsg )
initZoo =
    ( emptyZoo, Cmd.none )


emptyZoo : Zoo
emptyZoo =
    { elephantCount = 0, seaLionCount = 0, parakeetCount = 0 }


viewZoo : Zoo -> Html ZooMsg
viewZoo zoo =
    let
        population =
            viewPopulation
                [ ( "Elephant", zoo.elephantCount, onAddElephant )
                , ( "Sea Lion", zoo.seaLionCount, onAddSeaLion )
                , ( "Parakeet", zoo.parakeetCount, onAddParakeet )
                ]
                |> List.intersperse (br [] [])
    in
        div []
            (h4 [] [ text "Zoo population" ]
                :: population
                ++ [ br [] []
                   , button [ onClick ResetZoo ] [ text "Reset zoo" ]
                   ]
            )


viewPopulation : List ( String, Int, Zoo -> Zoo ) -> List (Html ZooMsg)
viewPopulation =
    List.map
        (\( beast, nb, cb ) ->
            div []
                [ text (beast ++ ": " ++ toString nb ++ " ")
                , button [ onClick (AddBeast cb) ] [ text ("Add " ++ beast) ]
                ]
        )


updateZoo : ZooMsg -> Zoo -> ( Zoo, Cmd ZooMsg )
updateZoo msg zoo =
    case msg of
        AddBeast f ->
            ( f zoo, Cmd.none )

        ResetZoo ->
            ( emptyZoo, Cmd.none )


onAddElephant : Zoo -> Zoo
onAddElephant zoo =
    { zoo | elephantCount = 1 + zoo.elephantCount }


onAddSeaLion : Zoo -> Zoo
onAddSeaLion zoo =
    { zoo | seaLionCount = 1 + zoo.seaLionCount }


onAddParakeet : Zoo -> Zoo
onAddParakeet zoo =
    { zoo | parakeetCount = 1 + zoo.parakeetCount }



--
