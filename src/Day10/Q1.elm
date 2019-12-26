port module Day10.Q1 exposing (..)

import Arithmetic exposing (gcd)
import Json.Encode as Encode exposing (Value)
import List as L
import List.Extra as LE
import Set exposing (Set)


port toJs : Value -> Cmd msg


type alias Flags =
    String


init : Flags -> ( (), Cmd msg )
init flags =
    ( ()
    , flags
        |> processInput
        |> calcRes1
        |> Maybe.map encoder
        |> Maybe.withDefault (Encode.string "error")
        |> toJs
    )



--


calcRes1 : List Point -> Maybe ( Point, Int )
calcRes1 asteroids =
    let
        mapper : Point -> ( Point, Int )
        mapper point =
            point
                |> getVectors asteroids
                |> reduceVectors
                |> Set.size
                |> Tuple.pair point
    in
    asteroids
        |> L.map mapper
        |> L.sortBy (Tuple.second >> (*) -1)
        |> L.head


getVectors : List Point -> Point -> List ( Int, Int )
getVectors list ( pX, pY ) =
    L.map (\( x, y ) -> ( x - pX, y - pY )) list


reduceVectors : List ( Int, Int ) -> Set ( Int, Int )
reduceVectors =
    let
        go : ( Int, Int ) -> Set ( Int, Int ) -> Set ( Int, Int )
        go vec acc =
            case vec of
                ( 0, 0 ) ->
                    acc

                ( 0, vY ) ->
                    Set.insert ( 0, sign vY ) acc

                ( vX, 0 ) ->
                    Set.insert ( sign vX, 0 ) acc

                _ ->
                    Set.insert (reduce vec) acc
    in
    L.foldl go Set.empty


sign x =
    if x <= 0 then
        -1

    else
        1


reduce : Point -> Point
reduce ( x, y ) =
    let
        gcd_ =
            gcd x y
    in
    ( x // gcd_, y // gcd_ )



--


encoder : ( Point, Int ) -> Value
encoder ( ( x, y ), tot ) =
    [ ( "x", Encode.int x )
    , ( "y", Encode.int y )
    , ( "tot", Encode.int tot )
    ]
        |> Encode.object


type alias Point =
    ( Int, Int )


processInput : String -> List Point
processInput string =
    let
        go : Int -> String -> List Point -> List Point
        go y line acc =
            processLine y (String.trim line) ++ acc
    in
    string
        |> String.split "\n"
        |> LE.indexedFoldl go []


processLine : Int -> String -> List Point
processLine y line =
    let
        go : Int -> Char -> List Point -> List Point
        go x s acc =
            if s == '#' then
                ( x, y ) :: acc

            else
                acc
    in
    line
        |> String.toList
        |> LE.indexedFoldl go []



--


main =
    Platform.worker
        { init = init
        , update = \_ _ -> ( (), Cmd.none )
        , subscriptions = \_ -> Sub.none
        }
