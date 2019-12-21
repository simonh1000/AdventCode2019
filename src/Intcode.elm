module Intcode exposing (..)

import Array exposing (Array)
import Common.CoreHelpers exposing (ifThenElse)



-- Modelling


type Step
    = Continue State
    | Stop State
    | Error String


type alias State =
    { input : List Int -- input values provided
    , output : Maybe Int -- last output
    , ptr : Int
    , arr : Array Int
    }


{-| required to pass 1 as input
-}
initState : State
initState =
    { input = []
    , output = Nothing
    , ptr = 0
    , arr = Array.empty
    }


type PMode
    = Pos -- 0
    | Imm -- 1


type alias Instruction =
    { op : Int
    , first : PMode
    , second : PMode
    , third : PMode
    }


mkInitState : String -> State
mkInitState str =
    { initState | arr = processInput str }


processInput : String -> Array Int
processInput =
    String.split ","
        >> List.filterMap String.toInt
        >> Array.fromList



--


runCode : Step -> Result String State
runCode step =
    case step of
        Continue state ->
            doStep state |> runCode

        Stop array ->
            Ok array

        Error err ->
            Err err


doStep : State -> Step
doStep state =
    let
        mbInstruction =
            Array.get state.ptr state.arr
                |> Maybe.map analyseInstruction

        --_ =
        --    Debug.log "doStep" <| pp state
    in
    case mbInstruction of
        Just ins ->
            case ins.op of
                99 ->
                    Stop state

                1 ->
                    doSum ins state

                2 ->
                    doMultiply ins state

                3 ->
                    doReadInput ins state

                4 ->
                    doOutput ins state

                5 ->
                    jumpTrue ins state

                6 ->
                    jumpFalse ins state

                7 ->
                    lessThan ins state

                8 ->
                    equals ins state

                _ ->
                    Error ("doStep 1 " ++ Debug.toString ins ++ "__" ++ Debug.toString state)

        _ ->
            Error ("doStep 2 " ++ Debug.toString state)


doSum : Instruction -> State -> Step
doSum ins state =
    let
        mbFirst =
            getAt ins.first (state.ptr + 1) state.arr

        mbSecond =
            getAt ins.second (state.ptr + 2) state.arr

        mbThird =
            Array.get (state.ptr + 3) state.arr

        --_ =
        --    Debug.log "Summing" ( ins, ( mbFirst, mbSecond, mbThird ) )
    in
    Maybe.map3 (\v1 v2 dst -> Array.set dst (v1 + v2) state.arr) mbFirst mbSecond mbThird
        |> Maybe.map (\arr_ -> Continue { state | ptr = state.ptr + 4, arr = arr_ })
        |> Maybe.withDefault (Error "doSum")


doMultiply : Instruction -> State -> Step
doMultiply ins state =
    let
        mbFirst =
            getAt ins.first (state.ptr + 1) state.arr

        mbSecond =
            getAt ins.second (state.ptr + 2) state.arr

        mbThird =
            Array.get (state.ptr + 3) state.arr

        --_ =
        --    Debug.log "Multiplying" ( ins, ( mbFirst, mbSecond, mbThird ) )
    in
    Maybe.map3 (\v1 v2 dst -> Array.set dst (v1 * v2) state.arr) mbFirst mbSecond mbThird
        |> Maybe.map (\arr_ -> Continue { state | ptr = state.ptr + 4, arr = arr_ })
        |> Maybe.withDefault (Error "doMultiply")


doReadInput : Instruction -> State -> Step
doReadInput ins state =
    let
        mbDst =
            Array.get (state.ptr + 1) state.arr

        --_ =
        --    Debug.log ("Insert input " ++ String.fromInt state.input) mbDst
    in
    case state.input of
        hd :: tl ->
            Maybe.map (\dst -> Array.set dst hd state.arr) mbDst
                |> Maybe.map (\arr_ -> Continue { state | ptr = state.ptr + 2, arr = arr_, input = tl })
                |> Maybe.withDefault (Error "doReadInput")

        [] ->
            Error "doReadInput: no input left"


doOutput : Instruction -> State -> Step
doOutput ins state =
    let
        output =
            getAt ins.first (state.ptr + 1) state.arr

        --_ =
        --    Debug.log "output" output
    in
    Continue { state | ptr = state.ptr + 2, output = output }


jumpTrue : Instruction -> State -> Step
jumpTrue ins state =
    let
        handler first second =
            if first == 0 then
                Continue { state | ptr = state.ptr + 3 }

            else
                Continue { state | ptr = second }
    in
    process2 handler "jumpTrue" ins state


jumpFalse : Instruction -> State -> Step
jumpFalse ins state =
    let
        handler first second =
            if first == 0 then
                Continue { state | ptr = second }

            else
                Continue { state | ptr = state.ptr + 3 }
    in
    process2 handler "jumpFalse" ins state


lessThan : Instruction -> State -> Step
lessThan ins state =
    let
        handler first second third =
            if first < second then
                Continue { state | ptr = state.ptr + 4, arr = Array.set third 1 state.arr }

            else
                Continue { state | ptr = state.ptr + 4, arr = Array.set third 0 state.arr }
    in
    process3 handler "jumpTrue" ins state


equals : Instruction -> State -> Step
equals ins state =
    let
        handler first second third =
            if first == second then
                Continue { state | ptr = state.ptr + 4, arr = Array.set third 1 state.arr }

            else
                Continue { state | ptr = state.ptr + 4, arr = Array.set third 0 state.arr }
    in
    process3 handler "equals" ins state


process2 : (Int -> Int -> Step) -> String -> Instruction -> State -> Step
process2 handler def ins state =
    let
        mbFirst =
            getAt ins.first (state.ptr + 1) state.arr

        mbSecond =
            getAt ins.second (state.ptr + 2) state.arr
    in
    Maybe.map2 handler mbFirst mbSecond
        |> Maybe.withDefault (Error def)


process3 : (Int -> Int -> Int -> Step) -> String -> Instruction -> State -> Step
process3 handler def ins state =
    let
        mbFirst =
            getAt ins.first (state.ptr + 1) state.arr

        mbSecond =
            getAt ins.second (state.ptr + 2) state.arr

        mbThird =
            --getAt ins.first (state.ptr + 3) state.arr
            Array.get (state.ptr + 3) state.arr

        --_ =
        --    Debug.log def ( mbFirst, mbSecond, mbThird )
    in
    Maybe.map3 handler mbFirst mbSecond mbThird
        |> Maybe.withDefault (Error def)



-- Helpers


pp : State -> String
pp state =
    state.arr
        |> Array.toList
        |> List.indexedMap
            (\idx v ->
                if idx == state.ptr then
                    "*" ++ String.fromInt v ++ "*"

                else
                    String.fromInt v
            )
        |> String.join ","


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
