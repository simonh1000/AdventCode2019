port module Day04.Q2 exposing (..)

import Json.Encode as Encode exposing (Value)
import List as L


port toJs : Value -> Cmd msg


myMin =
    372304


myMax =
    847060


init : String -> ( (), Cmd msg )
init string =
    ( ()
    , L.range myMin myMax
        |> L.filter checkNumber
        --|> L.length
        --|> Encode.int
        |> encoder
        |> toJs
    )


checkNumber n =
    [ 100000, 10000, 1000, 100, 10, 1 ]
        |> L.map (\i -> modBy 10 (n // i))
        |> test


test : List Int -> Bool
test lst =
    let
        go : Int -> ( Int, Validity ) -> ( Int, Validity )
        go d ( p1, b ) =
            --Debug.log "test" <|
            ( d
            , if b == Fails || d < p1 then
                Fails

              else
                case b of
                    HasPair p ->
                        if d == p then
                            Triple d

                        else
                            HasPair p

                    Triple p ->
                        if d == p then
                            b

                        else
                            Unknown

                    _ ->
                        if d == p1 then
                            HasPair d

                        else
                            b
            )
    in
    case lst of
        p1 :: tl ->
            L.foldl go ( p1, Unknown ) tl
                |> Tuple.second
                |> isHasPair

        _ ->
            False


type Validity
    = Unknown
    | HasPair Int
    | Triple Int
    | Fails


isHasPair v =
    case v of
        HasPair _ ->
            True

        _ ->
            False



--


encoder =
    Encode.list Encode.int


main =
    Platform.worker
        { init = init
        , update = \_ _ -> ( (), Cmd.none )
        , subscriptions = \_ -> Sub.none
        }
