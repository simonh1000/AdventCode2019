port module Day02.Q1 exposing (..)

import Array exposing (Array)
import Json.Encode as Encode exposing (Value)


port toJs : Value -> Cmd msg


init : String -> ( (), Cmd msg )
init string =
    ( ()
    , String.split "," string
        |> List.filterMap String.toInt
        |> Array.fromList
        |> inner 0
        |> Maybe.andThen (Array.get 0)
        |> Maybe.map Encode.int
        |> Maybe.withDefault (Encode.string "no index")
        |> toJs
    )


inner : Int -> Array Int -> Maybe (Array Int)
inner int array =
    case splitAt int array of
        Ok ic ->
            case applyStep ic array of
                Continue array_ ->
                    inner (int + 4) array_

                Stop array_ ->
                    Just array_

                Error err ->
                    Nothing

        Err err ->
            Nothing


applyStep : IntCode -> Array Int -> Step
applyStep ic array =
    case ic.opCode of
        1 ->
            -- +
            Maybe.map3 (\v1 v2 dst -> Array.set dst (v1 + v2) array) ic.val1 ic.val2 ic.dst
                |> Maybe.map Continue
                |> Maybe.withDefault (Error "applyStep")

        2 ->
            -- +
            Maybe.map3 (\v1 v2 dst -> Array.set dst (v1 * v2) array) ic.val1 ic.val2 ic.dst
                |> Maybe.map Continue
                |> Maybe.withDefault (Error "applyStep")

        99 ->
            Stop array

        _ ->
            Error <| "unexpected opcode, opCode = " ++ String.fromInt ic.opCode


splitAt : Int -> Array Int -> Result String IntCode
splitAt int array =
    Maybe.map
        (\opCode ->
            IntCode opCode
                (Array.get (int + 1) array |> Maybe.andThen (\idx -> Array.get idx array))
                (Array.get (int + 2) array |> Maybe.andThen (\idx -> Array.get idx array))
                (Array.get (int + 3) array)
        )
        (Array.get int array)
        |> Result.fromMaybe ("split at failed when int = " ++ String.fromInt int)


type alias IntCode =
    { opCode : Int
    , val1 : Maybe Int
    , val2 : Maybe Int
    , dst : Maybe Int
    }


type Step
    = Continue (Array Int)
    | Stop (Array Int)
    | Error String


main =
    Platform.worker
        { init = init
        , update = \_ _ -> ( (), Cmd.none )
        , subscriptions = \_ -> Sub.none
        }
