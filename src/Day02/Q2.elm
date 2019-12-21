port module Day02.Q2 exposing (..)

import Array exposing (Array)
import Json.Encode as Encode exposing (Value)


port toJs : Value -> Cmd msg


tgt =
    19690720


init : String -> ( (), Cmd msg )
init string =
    ( ()
    , String.split "," string
        |> List.filterMap String.toInt
        |> Array.fromList
        |> loopNoun
        |> Maybe.map (\( n, v, t ) -> Encode.list Encode.int [ n, v, t ])
        |> Maybe.withDefault (Encode.string "no index")
        |> toJs
    )


loopNoun : Array Int -> Maybe ( Int, Int, Int )
loopNoun array =
    let
        inner : Int -> Maybe (Array Int) -> Maybe (Array Int)
        inner int mbRes =
            case mbRes of
                Just arr ->
                    Just arr

                Nothing ->
                    --inner 19690720 0 array
                    if int == 99 then
                        Nothing

                    else
                        inner (Debug.log "loopNoun" <| int + 1) <| loopVerb (Array.set 1 int array)
    in
    inner 0 Nothing
        |> Maybe.andThen convertFinal


{-| here the noun will have been set by calling function
-}
loopVerb : Array Int -> Maybe (Array Int)
loopVerb array =
    let
        inner : Int -> Maybe (Array Int) -> Maybe (Array Int)
        inner int mbRes =
            case mbRes of
                Just arr ->
                    Just arr

                Nothing ->
                    --inner 19690720 0 array
                    if int == 99 then
                        Nothing

                    else
                        inner (Debug.log "loopVerb" <| int + 1) <| checkAnArray 0 (Array.set 2 int array)
    in
    inner 0 Nothing


checkAnArray : Int -> Array Int -> Maybe (Array Int)
checkAnArray int array =
    case splitAt int array of
        Ok ic ->
            case applyStep ic array of
                Continue array_ ->
                    if Array.get 0 array |> Maybe.map (\val -> val > tgt) |> Maybe.withDefault True then
                        Nothing

                    else
                        checkAnArray (int + 4) array_

                Stop array_ ->
                    if Array.get 0 array_ == Just tgt then
                        Just array_

                    else
                        Nothing

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


convertFinal : Array Int -> Maybe ( Int, Int, Int )
convertFinal array =
    Maybe.map2 (\noun verb -> ( noun, verb, 100 * noun + verb ))
        (Array.get 1 array)
        (Array.get 2 array)


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
