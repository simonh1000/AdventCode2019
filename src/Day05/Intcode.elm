module Intcode exposing (..)

import Array exposing (Array)
import Common.CoreHelpers exposing (debugWithFn, ifThenElse)



-- API


mkInitState : String -> State
mkInitState str =
    { initState | arr = processInput str }


runCode : State -> ProgState
runCode =
    Running >> runCodeInner


runCode2 : State -> Result String State
runCode2 state =
    case runCodeInner <| Running state of
        Running s ->
            Ok s

        Halted s ->
            Ok s

        EndedWithoutHalting _ ->
            Err "ended without halting"

        Error err ->
            Err err


type ProgState
    = Running State
    | EndedWithoutHalting State
    | Halted State
    | Error String


ppProgState : ProgState -> String
ppProgState progState =
    case progState of
        Running state ->
            "Running"

        EndedWithoutHalting state ->
            "EndedWithoutHalting"

        Halted state ->
            "Halted"

        Error string ->
            string


getState : ProgState -> Result String State
getState progState =
    case progState of
        Running state ->
            Ok state

        EndedWithoutHalting state ->
            Ok state

        Halted state ->
            Ok state

        Error string ->
            Err string


isRunning : ProgState -> Bool
isRunning progState =
    case progState of
        Running _ ->
            True

        EndedWithoutHalting _ ->
            True

        _ ->
            False


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



-- Modelling


type PMode
    = Pos -- 0
    | Imm -- 1


type alias Instruction =
    { op : Int
    , first : PMode
    , second : PMode
    , third : PMode
    }


processInput : String -> Array Int
processInput =
    String.split ","
        >> List.filterMap String.toInt
        >> Array.fromList



--


runCodeInner : ProgState -> ProgState
runCodeInner step =
    case step of
        Running state ->
            state
                |> debugWithFn pp
                |> doStep
                |> runCodeInner

        _ ->
            step


doStep : State -> ProgState
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
                    Halted state

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
            EndedWithoutHalting state


doSum : Instruction -> State -> ProgState
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
        |> Maybe.map (\arr_ -> Running { state | ptr = state.ptr + 4, arr = arr_ })
        |> Maybe.withDefault (Error "doSum")


doMultiply : Instruction -> State -> ProgState
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
        |> Maybe.map (\arr_ -> Running { state | ptr = state.ptr + 4, arr = arr_ })
        |> Maybe.withDefault (Error "doMultiply")


doReadInput : Instruction -> State -> ProgState
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
                |> Maybe.map (\arr_ -> Running { state | ptr = state.ptr + 2, arr = arr_, input = tl })
                |> Maybe.withDefault (Error "doReadInput")

        [] ->
            Error "doReadInput: no input left"


doOutput : Instruction -> State -> ProgState
doOutput ins state =
    let
        output =
            getAt ins.first (state.ptr + 1) state.arr

        --_ =
        --    Debug.log "output" output
    in
    Running { state | ptr = state.ptr + 2, output = output }


{-| Opcode 5 is jump-if-true: if the first parameter is non-zero,
it sets the instruction pointer to the value from the second parameter.
Otherwise, it does nothing
-}
jumpTrue : Instruction -> State -> ProgState
jumpTrue ins state =
    let
        handler first second =
            if first /= 0 then
                Running { state | ptr = second }

            else
                Running { state | ptr = state.ptr + 3 }
    in
    process2 handler "jumpTrue" ins state


jumpFalse : Instruction -> State -> ProgState
jumpFalse ins state =
    let
        handler first second =
            if first == 0 then
                Running { state | ptr = second }

            else
                Running { state | ptr = state.ptr + 3 }
    in
    process2 handler "jumpFalse" ins state


lessThan : Instruction -> State -> ProgState
lessThan ins state =
    let
        handler first second third =
            if first < second then
                Running { state | ptr = state.ptr + 4, arr = Array.set third 1 state.arr }

            else
                Running { state | ptr = state.ptr + 4, arr = Array.set third 0 state.arr }
    in
    process3 handler "jumpTrue" ins state


equals : Instruction -> State -> ProgState
equals ins state =
    let
        handler first second third =
            if first == second then
                Running { state | ptr = state.ptr + 4, arr = Array.set third 1 state.arr }

            else
                Running { state | ptr = state.ptr + 4, arr = Array.set third 0 state.arr }
    in
    process3 handler "equals" ins state


process2 : (Int -> Int -> ProgState) -> String -> Instruction -> State -> ProgState
process2 handler def ins state =
    let
        mbFirst =
            getAt ins.first (state.ptr + 1) state.arr

        mbSecond =
            getAt ins.second (state.ptr + 2) state.arr
    in
    Maybe.map2 handler mbFirst mbSecond
        |> Maybe.withDefault (Error def)


process3 : (Int -> Int -> Int -> ProgState) -> String -> Instruction -> State -> ProgState
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
        |> (++) ("inputs = " ++ Debug.toString state.input)


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
