module Day09.Intcode exposing (..)

import Array exposing (Array)
import Common.CoreHelpers exposing (debugWithFn, ifThenElse)
import List as L



-- API


runCode : State -> ProgState
runCode =
    Running >> runCodeInner


type ProgState
    = Running State
    | HitOutput Int State
    | Halted State
    | Error String


type alias State =
    { input : List Int -- input values provided
    , ptr : Int
    , arr : Array Int
    , relBase : Int
    }


mkInitState : String -> State
mkInitState str =
    { initState | arr = processInput str }


setInput : List Int -> State -> State
setInput input state =
    { state | input = input }


{-| required to pass 1 as input
-}
initState : State
initState =
    { input = []
    , ptr = 0
    , arr = Array.initialize 100 (\_ -> 0)
    , -- starts at 0
      relBase = 0
    }



-- init helper


processInput : String -> Array Int
processInput =
    String.split ","
        >> List.filterMap (String.toInt << String.trim)
        >> Array.fromList



--


{-| Runs until it hits a non-Running state
-}
runCodeInner : ProgState -> ProgState
runCodeInner step =
    case step of
        Running state ->
            let
                tmp =
                    doStep state
            in
            runCodeInner tmp

        --Running state ->
        --    state
        --        |> doStep
        --        |> runCodeInner
        --
        _ ->
            step


doStep : State -> ProgState
doStep state =
    let
        instruction =
            getA state.ptr state.arr
                |> analyseInstruction

        --_ =
        --    Debug.log "doStep" <| pp2 state
        --
        --_ =
        --    Debug.log "instruction" <| ppInstruction instruction
    in
    case instruction.op of
        99 ->
            Halted state

        1 ->
            doSum instruction state

        2 ->
            doMultiply instruction state

        3 ->
            doReadInput instruction state

        4 ->
            doOutput instruction state

        5 ->
            jumpTrue instruction state

        6 ->
            jumpFalse instruction state

        7 ->
            lessThan instruction state

        8 ->
            equals instruction state

        9 ->
            setRelativeBase instruction state

        _ ->
            Error ("doStep 1 " ++ Debug.toString instruction ++ "__" ++ Debug.toString state)


doSum : Instruction -> State -> ProgState
doSum ins state =
    let
        handler v1 v2 dst =
            Running
                { state
                    | arr = setA dst (v1 + v2) state.arr
                    , ptr = state.ptr + 4
                }
    in
    process3 handler ins state


doMultiply : Instruction -> State -> ProgState
doMultiply ins state =
    let
        handler v1 v2 dst =
            Running
                { state
                    | arr = setA dst (v1 * v2) state.arr
                    , ptr = state.ptr + 4
                }
    in
    process3 handler ins state


{-| reads a value from the input queue and inserts it at the dst position
-}
doReadInput : Instruction -> State -> ProgState
doReadInput ins state =
    let
        dst =
            getWritePtr ins.first (state.ptr + 1) state

        _ =
            Debug.log "Input instruction" ins

        _ =
            Debug.log ("Insert head of " ++ Debug.toString state.input ++ " at") dst
    in
    case state.input of
        hd :: tl ->
            Running
                { state
                    | arr = setA dst hd state.arr
                    , ptr = state.ptr + 2
                    , input = tl
                }

        [] ->
            Error "doReadInput: no input left"


doOutput : Instruction -> State -> ProgState
doOutput ins state =
    let
        output =
            getAt ins.first (state.ptr + 1) state

        --_ =
        --    Debug.log "output" output
    in
    HitOutput output { state | ptr = state.ptr + 2 }


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
    process2 handler ins state


jumpFalse : Instruction -> State -> ProgState
jumpFalse ins state =
    let
        handler first second =
            if first == 0 then
                Running { state | ptr = second }

            else
                Running { state | ptr = state.ptr + 3 }
    in
    process2 handler ins state


lessThan : Instruction -> State -> ProgState
lessThan ins state =
    let
        handler first second third =
            if first < second then
                Running
                    { state
                        | ptr = state.ptr + 4
                        , arr = setA third 1 state.arr
                    }

            else
                Running
                    { state
                        | ptr = state.ptr + 4
                        , arr = setA third 0 state.arr
                    }
    in
    process3 handler ins state


equals : Instruction -> State -> ProgState
equals ins state =
    let
        handler first second third =
            if first == second then
                Running
                    { state
                        | ptr = state.ptr + 4
                        , arr = setA third 1 state.arr
                    }

            else
                Running
                    { state
                        | ptr = state.ptr + 4
                        , arr = setA third 0 state.arr
                    }
    in
    process3 handler ins state


setRelativeBase : Instruction -> State -> ProgState
setRelativeBase ins state =
    let
        handler v =
            Running
                { state
                    | ptr = state.ptr + 2
                    , relBase = state.relBase + v
                }
    in
    handler (getAt ins.first (state.ptr + 1) state)


{-| first and second are NOT writes
-}
process2 : (Int -> Int -> ProgState) -> Instruction -> State -> ProgState
process2 handler ins state =
    let
        first =
            getAt ins.first (state.ptr + 1) state

        second =
            getAt ins.second (state.ptr + 2) state
    in
    handler first second


process3 : (Int -> Int -> Int -> ProgState) -> Instruction -> State -> ProgState
process3 handler ins state =
    let
        first =
            getAt ins.first (state.ptr + 1) state

        second =
            getAt ins.second (state.ptr + 2) state

        third =
            getWritePtr ins.third (state.ptr + 3) state

        --_ =
        --    Debug.log "Summing" ( ins, ( first, second, third ) )
    in
    handler first second third



-- -----------------------
-- Instruction
-- -----------------------


type alias Instruction =
    { op : Int
    , first : PMode
    , second : PMode -- not always used
    , third : PMode -- not always used
    }


analyseInstruction : Int -> Instruction
analyseInstruction i =
    { op = modBy 100 i
    , first = toPMode <| modBy 10 (i // 100)
    , second = toPMode <| modBy 10 (i // 1000)
    , third = toPMode <| modBy 10 (i // 10000)
    }


ppInstruction { op, first, second, third } =
    case op of
        99 ->
            "Halt"

        1 ->
            "doSum " ++ Debug.toString ( first, second, third )

        2 ->
            "doMultiply " ++ Debug.toString ( first, second, third )

        3 ->
            "doReadInput"

        4 ->
            "doOutput"

        5 ->
            "jumpTrue" ++ Debug.toString ( first, second )

        6 ->
            "jumpFalse" ++ Debug.toString ( first, second )

        7 ->
            "lessThan " ++ Debug.toString ( first, second, third )

        8 ->
            "equals " ++ Debug.toString ( first, second, third )

        9 ->
            "setRelativeBase " ++ Debug.toString first

        _ ->
            "**UNKNOWN OP**"



-- ---------------------
-- PMode
-- ---------------------


type PMode
    = Pos -- 0
    | Imm -- 1
    | Rel -- 2


getAt : PMode -> Int -> State -> Int
getAt pMode int state =
    case ( pMode, getA int state.arr ) of
        ( Pos, p ) ->
            getA p state.arr

        ( Imm, p ) ->
            p

        ( Rel, p ) ->
            getA (state.relBase + p) state.arr


getWritePtr : PMode -> Int -> State -> Int
getWritePtr pMode int state =
    case pMode of
        Pos ->
            getA int state.arr

        Imm ->
            Debug.todo "[getWritePtr] can't generate a pointer"

        Rel ->
            state.relBase + getA int state.arr


toPMode : Int -> PMode
toPMode int =
    case int of
        0 ->
            Pos

        1 ->
            Imm

        2 ->
            Rel

        _ ->
            Debug.todo ("toPMode could not handle " ++ String.fromInt int)



-- Helpers


getA : Int -> Array Int -> Int
getA int array =
    Array.get int array |> Maybe.withDefault 0


{-| if trying to insert beyond end of array, then extend with 0 until you can add
-}
setA : Int -> Int -> Array Int -> Array Int
setA idx v arr =
    if idx < 0 then
        arr

    else if idx >= Array.length arr then
        setA idx v (Array.push 0 arr)

    else
        Array.set idx v arr


getOutput : ProgState -> Maybe Int
getOutput progState =
    case progState of
        Running _ ->
            Nothing

        HitOutput int _ ->
            Just int

        Halted state ->
            -- if we hit halt, assume immediately after previous output
            state.input |> L.head

        Error _ ->
            Nothing


ppProgState : ProgState -> String
ppProgState progState =
    case progState of
        Running _ ->
            "Running"

        HitOutput o _ ->
            "Output: " ++ String.fromInt o

        Halted _ ->
            "Halted"

        Error string ->
            string


getState : ProgState -> Result String State
getState progState =
    case progState of
        Running state ->
            Ok state

        HitOutput _ state ->
            Ok state

        Halted state ->
            Ok state

        Error string ->
            Err string


isHalted : ProgState -> Bool
isHalted progState =
    case progState of
        Halted _ ->
            True

        _ ->
            False


pp2 : State -> String
pp2 state =
    "State length: " ++ String.fromInt (Array.length state.arr) ++ ", relBase=" ++ String.fromInt state.relBase ++ ", ptr=" ++ String.fromInt state.ptr


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
        --|> (++) ("inputs = " ++ Debug.toString state.input)
        |> (++) ("relBase=" ++ String.fromInt state.relBase ++ ", ")
