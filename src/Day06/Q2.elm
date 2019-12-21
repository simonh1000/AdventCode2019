port module Day06.Q2 exposing (..)

import Array exposing (Array)
import Basics.Extra exposing (flip)
import Common.CoreHelpers exposing (ifThenElse)
import Dict exposing (Dict)
import Json.Encode as Encode exposing (Value)


port toJs : Value -> Cmd msg



-- Modelling


type alias Constellation =
    Dict String (List String)


type alias State =
    { pathsExplored : Int
    , unexplored : List (List String)
    , santa : List String
    , you : List String
    }


initState : State
initState =
    { pathsExplored = 0
    , unexplored = [ [ "COM" ] ]
    , santa = []
    , you = []
    }



-- Init


init : String -> ( (), Cmd msg )
init string =
    let
        constellation =
            processInput string

        csum =
            checksum constellation 0 [ "COM" ]

        len =
            getLength constellation
    in
    ( ()
    , len |> Encode.int |> toJs
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


getLength constellation =
    let
        { santa, you } =
            explore constellation initState

        compRte r1 r2 =
            case ( r1, r2 ) of
                ( [], _ :: _ ) ->
                    List.length r2 - 1

                ( _ :: _, [] ) ->
                    List.length r1 - 1

                ( h1 :: t1, h2 :: t2 ) ->
                    if h1 == h2 then
                        compRte t1 t2

                    else
                        List.length t1 + List.length t2

                _ ->
                    -1
    in
    compRte (List.reverse santa) (List.reverse you)


checksum : Constellation -> Int -> List String -> Int
checksum constellation dist planets =
    --if List.length planets == 0 then
    --    0
    --
    --else
    --    let
    --        nextPlanets =
    --            List.foldl (\p acc -> Dict.get p constellation |> Maybe.withDefault [] |> (++) acc) [] planets
    --    in
    --    (dist * List.length planets) + checksum constellation (dist + 1) nextPlanets
    explore constellation initState
        |> Debug.log ""
        |> .pathsExplored


explore : Constellation -> State -> State
explore constellation state =
    let
        --_ =
        --    Debug.log "explore" state
        exploreTip : String -> List String -> List (List String) -> State
        exploreTip pathHead pathTail unexploredTail =
            let
                newUnexplored =
                    Dict.get pathHead constellation
                        |> Maybe.withDefault []
                        |> List.map (\nxt -> nxt :: pathHead :: pathTail)

                state_ =
                    { state
                        | pathsExplored = state.pathsExplored + 1
                        , unexplored = newUnexplored ++ unexploredTail
                    }
            in
            case pathHead of
                "SAN" ->
                    { state_ | santa = pathHead :: pathTail }
                        |> explore constellation

                "YOU" ->
                    { state_ | you = pathHead :: pathTail }
                        |> explore constellation

                _ ->
                    state_
                        |> explore constellation
    in
    case state.unexplored of
        [] ->
            -- no routes left - stop
            state

        unexploredHd :: unexploredTail ->
            case unexploredHd of
                [] ->
                    -- impossible state
                    state

                pathHd :: pathTl ->
                    exploreTip pathHd pathTl unexploredTail



-- program


main =
    Platform.worker
        { init = init
        , update = \_ _ -> ( (), Cmd.none )
        , subscriptions = \_ -> Sub.none
        }
