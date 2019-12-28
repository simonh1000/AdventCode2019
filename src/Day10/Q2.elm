port module Day10.Q2 exposing (..)

import Arithmetic exposing (gcd)
import Common.CoreHelpers exposing (ifThenElse)
import Dict exposing (Dict)
import Dict.Extra as DE
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
        |> processInput
        |> calcRes2
        |> encoder
        |> toJs
    )


tgt =
    ( 20, 18 )



--


calcRes2 asteroids =
    getVectors asteroids
        |> Dict.toList
        |> L.map (Tuple.mapFirst normaliseVector)
        |> L.sortBy Tuple.first
        |> L.map Tuple.second
        |> orderSubLists
        |> putInOrder


putInOrder : List (List ( Int, Int )) -> List ( Int, Int )
putInOrder lists =
    if lists == [] then
        []

    else
        L.filterMap L.head lists ++ putInOrder (L.filterMap L.tail lists)


orderSubLists : List (List ( Int, Int )) -> List (List ( Int, Int ))
orderSubLists =
    let
        ( tgtX, tgtY ) =
            tgt

        dst ( x, y ) =
            (x - tgtX) ^ 2 + (y - tgtY) ^ 2
    in
    L.map (L.sortBy dst)


getVectors : List Point -> Dict ( Int, Int ) (List ( Int, Int ))
getVectors list =
    let
        ( tgtX, tgtY ) =
            tgt

        insert_ k v acc =
            case Dict.get k acc of
                Just vs ->
                    Dict.insert k (v :: vs) acc

                Nothing ->
                    Dict.insert k [ v ] acc

        go : ( Int, Int ) -> Dict ( Int, Int ) (List ( Int, Int )) -> Dict ( Int, Int ) (List ( Int, Int ))
        go (( x, y ) as pt) acc =
            case ( x - tgtX, tgtY - y ) of
                ( 0, 0 ) ->
                    acc

                ( 0, vY ) ->
                    insert_ ( 0, sign vY ) pt acc

                ( vX, 0 ) ->
                    insert_ ( sign vX, 0 ) pt acc

                vec ->
                    insert_ (reduce vec) pt acc
    in
    L.foldl go Dict.empty list


normaliseVector : ( Int, Int ) -> Float
normaliseVector vec =
    let
        rads =
            case vec of
                ( 0, vY ) ->
                    ifThenElse (vY > 0) 0 pi

                ( vX, 0 ) ->
                    ifThenElse (vX > 0) (pi / 2) (3 * pi / 2)

                ( vX, vY ) ->
                    if vX < 0 then
                        3 * pi / 2 - atan (toFloat vY / toFloat vX)

                    else
                        pi / 2 - atan (toFloat vY / toFloat vX)
    in
    rads / 2 / pi * 360


sign : number -> number
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


encoder : List ( Int, Int ) -> Value
encoder =
    L.indexedMap (\idx pt -> String.fromInt (idx + 1) ++ Debug.toString pt)
        >> String.join "\n"
        >> Encode.string



--encoder ( x, y ) =
--    x * 100 + y |> Encode.int


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
