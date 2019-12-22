port module Day07.Q2 exposing (..)

import Day07.Intcode exposing (..)
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



--


type alias Amplifiers =
    List ProgState


tryPermutations : State -> Maybe ( List Int, Int )
tryPermutations initState =
    L.range 5 9
        |> LE.permutations
        --|> L.singleton
        |> L.map (\perm -> ( perm, Tuple.first <| runUntilHalted <| mkPermutation initState perm ))
        |> L.sortBy (Tuple.second >> (*) -1)
        |> L.head


mkPermutation : State -> List Int -> Amplifiers
mkPermutation initState =
    L.map (\i -> Running { initState | input = [ i ] })


{-| -}
runUntilHalted : Amplifiers -> ( Int, Amplifiers )
runUntilHalted amplifiers =
    let
        iterate : Int -> Amplifiers -> ( Int, Amplifiers )
        iterate int amps =
            let
                ( out, amps_ ) =
                    loopOnce int amps
            in
            if L.all isHalted amps_ then
                ( out, amps_ )

            else
                iterate out amps_
    in
    iterate 0 amplifiers


loopOnce : Int -> Amplifiers -> ( Int, Amplifiers )
loopOnce inp amplifiers =
    let
        go : ProgState -> ( Int, Amplifiers ) -> ( Int, Amplifiers )
        go progState ( previous, accAmps ) =
            let
                handler state =
                    let
                        next =
                            runCode { state | input = state.input ++ [ previous ] }
                    in
                    case getOutput next of
                        Just nextOutput ->
                            ( nextOutput, accAmps ++ [ next ] )

                        Nothing ->
                            Debug.todo "err"
            in
            case getState progState of
                Ok state ->
                    handler state

                _ ->
                    Debug.todo "Attempting to run a program that is not running"
    in
    L.foldl go ( inp, [] ) amplifiers


main =
    Platform.worker
        { init = init
        , update = \_ _ -> ( (), Cmd.none )
        , subscriptions = \_ -> Sub.none
        }
