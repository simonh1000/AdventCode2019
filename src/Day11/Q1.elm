port module Day11.Q1 exposing (..)

import Common.CoreHelpers exposing (ifThenElse)
import Day11.Intcode exposing (..)
import Dict exposing (Dict)
import Json.Encode as Encode exposing (Value)
import List as L


port toJs : Value -> Cmd msg


type alias Flags =
    String


init : Flags -> ( (), Cmd msg )
init flags =
    ( ()
    , flags
        |> initState
        |> doIteration
        |> encoder
        |> toJs
    )



--


type alias Ship =
    Dict ( Int, Int ) Colour


pp : Ship -> String
pp s =
    let
        ppColor c =
            ifThenElse (c == Black) "." "#"
    in
    L.range -3 3
        |> L.map
            (\x ->
                L.range -3 3
                    |> L.map (\y -> getShipCol s ( x, y ) |> ppColor)
                    |> String.join ""
            )
        |> String.join "\n"


getShipCol ship pos =
    Dict.get pos ship |> Maybe.withDefault Black


type Colour
    = Black
    | White


type Direction
    = U
    | D
    | L
    | R


{-| provide 0 if the robot is over a black panel or 1 if the robot is over a white panel
-}
c2i : Colour -> number
c2i c =
    case c of
        Black ->
            0

        White ->
            1


i2c : Int -> Colour
i2c int =
    case int of
        0 ->
            Black

        1 ->
            White

        _ ->
            Debug.todo "bad int to i2c"


type alias Q11State =
    { ship : Ship
    , computer : State
    , pos : ( Int, Int )
    , dir : Direction
    , changes : Int
    }


initState : String -> Q11State
initState inp =
    { ship = Dict.empty
    , computer = mkInitState inp
    , pos = ( 0, 0 )
    , dir = U
    , changes = 0
    }


right : Direction -> Direction
right d =
    case d of
        U ->
            R

        D ->
            L

        L ->
            U

        R ->
            D


left : Direction -> Direction
left d =
    case d of
        U ->
            L

        D ->
            R

        L ->
            D

        R ->
            U


move : Direction -> ( Int, Int ) -> ( Int, Int )
move direction ( x, y ) =
    case direction of
        U ->
            ( x, y + 1 )

        D ->
            ( x, y - 1 )

        L ->
            ( x - 1, y )

        R ->
            ( x + 1, y )



--


doIteration : Q11State -> Q11State
doIteration qstate =
    let
        currCol =
            Dict.get qstate.pos qstate.ship |> Maybe.withDefault Black

        computer =
            setInput [ c2i currCol ] qstate.computer
    in
    case get2Outputs computer of
        Just ( intCol, intDir, computer_ ) ->
            let
                newCol =
                    i2c intCol

                newDir =
                    -- 0 means it should turn left 90 degrees, and 1 means it should turn right 90 degrees
                    ifThenElse (intDir == 0) (left qstate.dir) (right qstate.dir)

                changes =
                    if qstate.pos == ( 0, 0 ) || (newCol == currCol) then
                        qstate.changes

                    else
                        qstate.changes + 1

                newState =
                    { qstate
                        | computer = computer_
                        , ship = Dict.insert qstate.pos newCol qstate.ship
                        , changes = changes
                        , pos = move newDir qstate.pos
                        , dir = newDir
                    }
            in
            doIteration newState

        Nothing ->
            qstate


get2Outputs : State -> Maybe ( Int, Int, State )
get2Outputs computer =
    case runCodeInner <| Running computer of
        HitOutput int1 computer1 ->
            case runCodeInner <| Running computer1 of
                HitOutput int2 computer2 ->
                    Just ( int1, int2, computer2 )

                _ ->
                    Debug.todo "phase 2 did not return a value"

        Halted _ ->
            Nothing

        Error err ->
            Debug.todo err

        Running state ->
            Debug.todo "unexpectedly received a running state"



--


encoder : Q11State -> Value
encoder s =
    [ ( "ship", Encode.string <| pp s.ship )
    , ( "changes", Encode.int <| Dict.size s.ship )
    ]
        |> Encode.object


main =
    Platform.worker
        { init = init
        , update = \_ _ -> ( (), Cmd.none )
        , subscriptions = \_ -> Sub.none
        }
