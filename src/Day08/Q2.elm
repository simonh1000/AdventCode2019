port module Day08.Q2 exposing (..)

import Array exposing (Array)
import Common.CoreHelpers exposing (convertX)
import Day08.Q1 exposing (convertX)
import Json.Encode as Encode exposing (Value)
import List as L
import List.Extra as LE


port toJs : Value -> Cmd msg


type alias Flags =
    { w : Int
    , t : Int
    , img : String
    }


init : Flags -> ( (), Cmd msg )
init flags =
    ( ()
    , flags
        |> processInput
        |> getResult flags
        |> resToVal flags
        |> toJs
    )


processInput : Flags -> List Int
processInput flags =
    let
        charToInt c =
            Char.toCode c - Char.toCode '0'
    in
    flags.img |> String.toList |> L.map charToInt


getResult : Flags -> List Int -> Array Int
getResult flags =
    let
        sz =
            flags.w * flags.t

        go : Int -> Int -> Array Int -> Array Int
        go idx val acc =
            let
                m =
                    modBy sz idx
            in
            if Array.get m acc == Just 2 then
                Array.set m val acc

            else
                acc
    in
    LE.indexedFoldl go (Array.initialize sz (\_ -> 2))


resToVal : Flags -> Array Int -> Value
resToVal flags array =
    array
        |> Array.toList
        |> L.map String.fromInt
        |> String.join ""
        |> readLayer flags 0
        |> Tuple.second
        |> String.join "\n"
        |> Encode.string


readLayer : Flags -> Int -> String -> ( String, List String )
readLayer f h string =
    if string == "" || h == f.t then
        ( string, [] )

    else
        readLayer f (h + 1) (String.dropLeft f.w string)
            |> Tuple.mapSecond ((::) (convertX (String.left f.w string)))


main =
    Platform.worker
        { init = init
        , update = \_ _ -> ( (), Cmd.none )
        , subscriptions = \_ -> Sub.none
        }
