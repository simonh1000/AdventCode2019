port module Day07.Q1 exposing (..)

import Intcode exposing (..)
import Json.Encode as Encode exposing (Value)
import List as L
import List.Extra as LE


port toJs : Value -> Cmd msg


type alias Flags =
    String


init : Flags -> ( (), Cmd msg )
init flags =
    ( ()
    , flags
        |> mkInitState
        |> tryPermutations
        |> Maybe.map encoder
        |> Maybe.withDefault (Encode.string "unexpected")
        |> toJs
    )


encoder : ( List Int, Int ) -> Value
encoder ( ints, val ) =
    [ ( "ints", Encode.list Encode.int ints )
    , ( "val", Encode.int val )
    ]
        |> Encode.object


tryPermutations : State -> Maybe ( List Int, Int )
tryPermutations state =
    L.range 0 4
        |> LE.permutations
        |> L.map (\perm -> ( perm, tryPermutation state perm ))
        |> L.sortBy (Tuple.second >> (*) -1)
        |> L.head


tryPermutation : State -> List Int -> Int
tryPermutation state ints =
    let
        go : Int -> Result String Int -> Result String Int
        go phase mbAcc =
            case mbAcc of
                Ok acc ->
                    { state | input = [ phase, acc ] }
                        |> runCode
                        |> Result.andThen (.output >> Result.fromMaybe "No Output")

                err ->
                    err
    in
    L.foldl go (Ok 0) ints
        |> Result.withDefault -1


main =
    Platform.worker
        { init = init
        , update = \_ _ -> ( (), Cmd.none )
        , subscriptions = \_ -> Sub.none
        }
