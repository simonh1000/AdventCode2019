port module Day09.Q1 exposing (..)

import Day09.Intcode as IC exposing (..)
import Json.Encode as Encode exposing (Value)


port toJs : Value -> Cmd msg


type alias Flags =
    String


init : Flags -> ( (), Cmd msg )
init flags =
    ( ()
    , flags
        |> mkInitState
        |> setInput [ 2 ]
        |> runCode
        |> (\_ -> Encode.string "Done")
        |> toJs
    )


runCode : State -> ProgState
runCode state =
    case IC.runCode state of
        HitOutput r state_ ->
            let
                _ =
                    Debug.log "-->" r
            in
            runCode state_

        x ->
            let
                _ =
                    Debug.log "" x
            in
            x


main =
    Platform.worker
        { init = init
        , update = \_ _ -> ( (), Cmd.none )
        , subscriptions = \_ -> Sub.none
        }
