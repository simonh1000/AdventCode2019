port module Day05.Q1 exposing (..)

import Array exposing (Array)
import Basics.Extra exposing (flip)
import Common.CoreHelpers exposing (ifThenElse)
import Json.Encode as Encode exposing (Value)


port toJs : Value -> Cmd msg



-- Modelling


type Step
    = Continue State
    | Stop (Array Int)
    | Error String


type alias State =
    { input : Int
    , ptr : Maybe Int
    , arr : Array Int
    }


initState =
    State 0 (Just 0) Array.empty


type PMode
    = Pos -- 0
    | Imm -- 1


type alias Instruction =
    { op : Int
    , first : PMode
    , second : PMode
    , third : PMode
    }


init : String -> ( (), Cmd msg )
init string =
    ( ()
    , string
        |> processInput
        |> (\arr ->
                { initState | arr = arr }
                    |> Continue
                    |> looper
                    |> toJs
           )
    )


processInput : String -> Array Int
processInput =
    String.split ","
        >> List.filterMap String.toInt
        >> Array.fromList


looper : Step -> Value
looper step =
    case Debug.log "looper" step of
        Continue state ->
            doStep state
                |> looper

        Stop array ->
            array
                |> Array.get 0
                |> Maybe.map Encode.int
                |> Maybe.withDefault (Encode.string "Stop")

        Error err ->
            Encode.string err


getAt : PMode -> Int -> Array Int -> Maybe Int
getAt pMode int array =
    case ( pMode, Array.get int array ) of
        ( Pos, Just p ) ->
            Array.get p array

        ( Imm, p ) ->
            p

        _ ->
            Nothing


analyseInstruction : Int -> Instruction
analyseInstruction i =
    let
        conv b =
            ifThenElse (b == 0) Pos Imm
    in
    { op = modBy 100 i
    , first = conv <| modBy 10 (i // 100)
    , second = conv <| modBy 10 (i // 1000)
    , third = conv <| modBy 10 (i // 10000)
    }


doStep : State -> Step
doStep state =
    let
        mbPtrVal =
            state.ptr
                |> Maybe.andThen (flip Array.get state.arr)
                |> Maybe.map analyseInstruction
    in
    case ( state.ptr, mbPtrVal ) of
        ( Just ptr_, Just ptrVal ) ->
            case ptrVal.op of
                99 ->
                    Stop state.arr

                1 ->
                    doSum ptr_ ptrVal state

                2 ->
                    doMultiply ptr_ ptrVal state

                _ ->
                    Error ("doStep" ++ Debug.toString state)

        _ ->
            Error ("doStep" ++ Debug.toString state)


doSum : Int -> Instruction -> State -> Step
doSum ptr ins s =
    Maybe.map3 (\v1 v2 dst -> Array.set dst (v1 + v2) s.arr)
        (getAt ins.first (ptr + 1) s.arr)
        (getAt ins.second (ptr + 2) s.arr)
        (Array.get (ptr + 3) s.arr)
        |> Maybe.map (\arr_ -> Continue { s | ptr = Just <| ptr + 4, arr = arr_ })
        |> Maybe.withDefault (Error "doSum")


doMultiply : Int -> Instruction -> State -> Step
doMultiply ptr ins s =
    Maybe.map3 (\v1 v2 dst -> Array.set dst (v1 * v2) s.arr)
        (getAt ins.first (ptr + 1) s.arr)
        (getAt ins.second (ptr + 2) s.arr)
        (Array.get (ptr + 3) s.arr)
        |> Maybe.map (\arr_ -> Continue { s | ptr = Just <| ptr + 4, arr = arr_ })
        |> Maybe.withDefault (Error "doMultiply")


main =
    Platform.worker
        { init = init
        , update = \_ _ -> ( (), Cmd.none )
        , subscriptions = \_ -> Sub.none
        }
