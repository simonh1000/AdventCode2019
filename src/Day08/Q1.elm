port module Day08.Q1 exposing (..)

import Common.CoreHelpers exposing (ifThenElse)
import Json.Encode as Encode exposing (Value)
import List as L


port toJs : Value -> Cmd msg


type alias Flags =
    { w : Int
    , t : Int
    , img : String
    }


init : Flags -> ( (), Cmd msg )
init flags =
    ( ()
    , flags
        |> processInput
        |> getQ1Result
        |> Encode.int
        |> toJs
    )


type alias Layer =
    List (List Int)


type alias Layers =
    List Layer


processInput : Flags -> Layers
processInput flags =
    let
        charToInt c =
            Char.toCode c - Char.toCode '0'

        convertX str =
            str |> String.toList |> L.map charToInt

        readLayer : Int -> String -> ( String, List (List Int) )
        readLayer h string =
            if string == "" || h == flags.t then
                ( string, [] )

            else
                readLayer (h + 1) (String.dropLeft flags.w string)
                    |> Tuple.mapSecond ((::) (convertX (String.left flags.w string)))

        readLayers str layers =
            let
                ( remainder, layer ) =
                    readLayer 0 str

                layers_ =
                    layers ++ [ layer ]
            in
            if remainder == "" then
                layers_

            else
                readLayers remainder layers_
    in
    readLayers flags.img []


getQ1Result : Layers -> Int
getQ1Result layers =
    layers
        |> getLayerWithFewestZeros
        |> checkSum


checkSum : Layer -> Int
checkSum layer =
    countTgt 1 layer * countTgt 2 layer


getLayerWithFewestZeros : Layers -> List (List Int)
getLayerWithFewestZeros layers =
    layers
        |> L.sortBy (countTgt 0)
        |> L.head
        |> Maybe.withDefault []
        |> Debug.log "getLayerWithFewestZeros"


countTgt : Int -> Layer -> Int
countTgt tgt layer =
    let
        go x acc =
            ifThenElse (x == tgt) (acc + 1) acc
    in
    layer |> L.concat |> L.foldl go 0


main =
    Platform.worker
        { init = init
        , update = \_ _ -> ( (), Cmd.none )
        , subscriptions = \_ -> Sub.none
        }
