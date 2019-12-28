port module Day03.Q1 exposing (..)

import Basics.Extra exposing (flip)
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
                Encode.string "Something went wrong"
    )


doCalculation : String -> Maybe Int
doCalculation string =
    convertFlags string
        |> Tuple.mapFirst getPointsFromLine
        |> Tuple.mapSecond getPointsFromLine
        |> findNearest


findNearest : ( List Point, List Point ) -> Maybe Int
findNearest ( p1, p2 ) =
    Set.intersect (Set.fromList p1) (Set.fromList p2)
        |> Set.toList
        |> L.map (\( x, y ) -> abs x + abs y)
        |> L.minimum


getPointsFromLine : List Move -> List Point
getPointsFromLine moves =
    let
        handleMove : Move -> ( Point, List Point ) -> ( Point, List Point )
        handleMove move ( currPos, acc ) =
            let
                handler mapper adder idx =
                    L.range 1 idx
                        |> L.map (\y -> mapper (adder y) currPos)
                        |> (++) acc
                        |> Tuple.pair (mapper (adder idx) currPos)
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
    L.foldl handleMove ( ( 0, 0 ), [] ) moves
        |> Tuple.second


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
