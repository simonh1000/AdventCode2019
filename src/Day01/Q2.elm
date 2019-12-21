port module Day01.Q2 exposing (..)

import Dict exposing (Dict)


port toJs : Int -> Cmd msg


init : String -> ( (), Cmd msg )
init string =
    let
        data =
            string
                |> String.split "\n"
                |> List.filterMap String.toInt
                |> List.sort
    in
    ( ()
    , data |> foldRec |> toJs
    )


foldRec : List Int -> Int
foldRec lst =
    let
        go : Int -> ( Int, Dict Int Int ) -> ( Int, Dict Int Int )
        go mass ( accFuel, accDict ) =
            getFuelRecursive mass accDict
                |> Tuple.mapFirst ((+) accFuel)
    in
    List.foldl go ( 0, initAcc ) lst
        |> Tuple.first


initAcc =
    Dict.singleton 0 0


getFuelRecursive : Int -> Dict Int Int -> ( Int, Dict Int Int )
getFuelRecursive mass acc =
    case Dict.get mass acc of
        Just res ->
            ( res, acc )

        Nothing ->
            let
                massThis =
                    getFuel mass

                ( massRec, accRec ) =
                    getFuelRecursive massThis acc

                total =
                    massThis + massRec
            in
            ( total, Dict.insert mass total accRec )


getFuel : Int -> Int
getFuel mass =
    max 0 (mass // 3 - 2)


main =
    Platform.worker
        { init = init
        , update = \_ _ -> ( (), Cmd.none )
        , subscriptions = \_ -> Sub.none
        }
