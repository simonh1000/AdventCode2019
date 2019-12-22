port module Day06.Q1 exposing (..)

import Basics.Extra exposing (flip)
import Dict exposing (Dict)
import Json.Encode as Encode exposing (Value)


port toJs : Value -> Cmd msg



-- Modelling


type alias Constellation =
    Dict String (List String)



-- Init


init : String -> ( (), Cmd msg )
init string =
    let
        constellation =
            processInput string

        csum =
            checksum constellation 0 [ "COM" ]
    in
    ( ()
    , csum |> Encode.int |> toJs
    )


processInput str =
    let
        go : String -> Constellation -> Constellation
        go line acc =
            case String.split ")" (String.trim line) of
                [ a, b ] ->
                    acc
                        |> Dict.get a
                        |> Maybe.withDefault []
                        |> (::) b
                        |> flip (Dict.insert a) acc

                _ ->
                    Debug.todo "could not parse"
    in
    str
        |> String.split "\n"
        |> List.foldl go Dict.empty



-- execution


checksum : Constellation -> Int -> List String -> Int
checksum constellation dist planets =
    if List.length planets == 0 then
        0

    else
        let
            nextPlanets =
                List.foldl (\p acc -> Dict.get p constellation |> Maybe.withDefault [] |> (++) acc) [] planets
        in
        (dist * List.length planets) + checksum constellation (dist + 1) nextPlanets



-- program


main =
    Platform.worker
        { init = init
        , update = \_ _ -> ( (), Cmd.none )
        , subscriptions = \_ -> Sub.none
        }
