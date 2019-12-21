port module Day03.Q2 exposing (..)

import Basics.Extra exposing (flip)
import Dict exposing (Dict)
import Json.Encode as Encode exposing (Value)
import List as L
import Set exposing (Set)


port toJs : Value -> Cmd msg


init : String -> ( (), Cmd msg )
init string =
    ( ()
    , toJs <|
        case doCalculation string of
            Just ans ->
                Encode.int ans

            Nothing ->
                Encode.string "****** Something went wrong"
    )


doCalculation : String -> Maybe Int
doCalculation string =
    convertFlags string
        |> Tuple.mapFirst getPointsFromLine
        |> Tuple.mapSecond getPointsFromLine
        |> findNearest


findNearest : ( Dict Point Int, Dict Point Int ) -> Maybe Int
findNearest ( p1, p2 ) =
    let
        meetingPoints =
            Set.intersect (Set.fromList <| Dict.keys p1) (Set.fromList <| Dict.keys p2)
                |> Set.toList

        evaluator ( x, y ) =
            abs x + abs y

        evaluator2 p =
            Maybe.map2 (+) (Dict.get p p1) (Dict.get p p2)
    in
    meetingPoints
        |> L.filterMap evaluator2
        |> L.minimum


getPointsFromLine : List Move -> Dict Point Int
getPointsFromLine moves =
    let
        handleMove : Move -> ( Int, Point, Dict Point Int ) -> ( Int, Point, Dict Point Int )
        handleMove move ( distSoFar, currPos, acc ) =
            let
                handler : ((Int -> Int) -> (Point -> Point)) -> (Int -> Int -> Int) -> Int -> ( Int, Point, Dict Point Int )
                handler mapper adder idx =
                    ( distSoFar + idx
                    , mapper (adder idx) currPos
                    , L.range 1 idx
                        |> L.foldl (addMoveToDict mapper adder distSoFar currPos) acc
                    )
            in
            case move of
                U idx ->
                    handler Tuple.mapSecond (+) idx

                D idx ->
                    handler Tuple.mapSecond (flip (-)) idx

                R idx ->
                    handler Tuple.mapFirst (+) idx

                L idx ->
                    handler Tuple.mapFirst (flip (-)) idx
    in
    L.foldl handleMove ( 0, ( 0, 0 ), Dict.empty ) moves
        |> (\( _, _, lst ) -> lst)


addMoveToDict : ((Int -> Int) -> (Point -> Point)) -> (Int -> Int -> Int) -> Int -> Point -> Int -> Dict Point Int -> Dict Point Int
addMoveToDict mapper adder distSoFar currPos ct acc =
    let
        pos =
            mapper (adder ct) currPos
    in
    Dict.insert pos (distSoFar + ct) acc


type alias Point =
    ( Int, Int )


type Move
    = U Int
    | R Int
    | D Int
    | L Int


convertFlags : String -> ( List Move, List Move )
convertFlags str =
    case str |> String.split "\n" |> L.map convertLine of
        [ l1, l2 ] ->
            ( l1, l2 )

        _ ->
            Debug.todo "bad input"


convertLine : String -> List Move
convertLine str =
    str |> String.split "," |> L.filterMap convertItem


convertItem : String -> Maybe Move
convertItem str =
    let
        mbInt =
            String.toInt (String.dropLeft 1 str)

        mbMove =
            case String.left 1 str of
                "U" ->
                    Just U

                "R" ->
                    Just R

                "D" ->
                    Just D

                "L" ->
                    Just L

                _ ->
                    Nothing
    in
    Maybe.map2 (|>) mbInt mbMove


main =
    Platform.worker
        { init = init
        , update = \_ _ -> ( (), Cmd.none )
        , subscriptions = \_ -> Sub.none
        }
