port module Day04.Q1 exposing (..)

import Basics.Extra exposing (flip)
import Common.CoreHelpers exposing (ifThenElse)
import Json.Encode as Encode exposing (Value)
import List as L
import Set exposing (Set)


port toJs : Value -> Cmd msg


myMin =
    372304


myMax =
    847060


init : String -> ( (), Cmd msg )
init string =
    ( ()
    , getCandidates
        |> L.length
        |> Encode.int
        --|> encoder
        |> toJs
    )


getCandidates =
    L.range myMin myMax
        |> L.filter checkNumber



--|> L.length


checkNumber n =
    [ 100000, 10000, 1000, 100, 10, 1 ]
        |> L.map (\i -> modBy 10 (n // i))
        |> hasTwoSame


hasTwoSame : List Int -> Bool
hasTwoSame lst =
    let
        go : comparable -> ( comparable, Validity ) -> ( comparable, Validity )
        go d ( p, b ) =
            ( d
            , if b == FailsIncreasing || d < p then
                FailsIncreasing

              else if d == p then
                HasPair

              else
                b
            )
    in
    case lst of
        hd :: tl ->
            L.foldl go ( hd, Unknown ) tl |> Tuple.second |> (==) HasPair

        _ ->
            False


type Validity
    = Unknown
    | HasPair
    | FailsIncreasing



--


encoder =
    Debug.toString >> Encode.string


main =
    Platform.worker
        { init = init
        , update = \_ _ -> ( (), Cmd.none )
        , subscriptions = \_ -> Sub.none
        }
