port module Day05.Q2 exposing (..)

import Array exposing (Array)
import Common.CoreHelpers exposing (ifThenElse)
import Intcode exposing (..)
import Json.Encode as Encode exposing (Value)


port toJs : Value -> Cmd msg


type alias Flags =
    { input : Int
    , code : String
    }


init : Flags -> ( (), Cmd msg )
init flags =
    ( ()
    , flags.code
        |> processInput
        |> (\arr ->
                { initState | arr = arr, input = [ flags.input ] }
                    |> Continue
                    |> looper
                    |> toJs
           )
    )


looper : Step -> Value
looper step =
    case step of
        Continue state ->
            doStep state
                |> looper

        Stop array ->
            array
                |> Array.get 0
                |> Maybe.map Encode.int
                |> Maybe.withDefault (Encode.string "Stop")

        Error err ->
            Encode.string <| "Error: " ++ err


processInput : String -> Array Int
processInput =
    String.split ","
        >> List.filterMap String.toInt
        >> Array.fromList


main =
    Platform.worker
        { init = init
        , update = \_ _ -> ( (), Cmd.none )
        , subscriptions = \_ -> Sub.none
        }
