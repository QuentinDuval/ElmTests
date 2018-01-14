module Elementary exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Dict exposing (Dict)


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
    { population = Dict.fromList (List.map (\b -> ( b, 0 )) species) }


viewZoo : Zoo -> Html ZooMsg
viewZoo zoo =
    let
        population =
            List.intersperse (br [] []) (viewPopulation zoo)
    in
        div []
            (h4 [] [ text "Zoo population" ]
                :: population
                ++ [ br [] []
                   , button [ onClick ResetZoo ] [ text "Reset zoo" ]
                   ]
            )


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
