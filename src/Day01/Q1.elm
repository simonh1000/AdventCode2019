port module Day01.Q1 exposing (..)


port toJs : Int -> Cmd msg


init : String -> ( (), Cmd msg )
init string =
    ( ()
    , String.split "\n" string |> folder |> toJs
    )


folder : List String -> Int
folder =
    let
        go : String -> Int -> Int
        go string acc =
            String.toInt string
                |> Maybe.map (getFuel >> (+) acc)
                |> Maybe.withDefault acc
    in
    List.foldl go 0


getFuel : Int -> Int
getFuel mass =
    mass // 3 - 2


main =
    Platform.worker
        { init = init
        , update = \_ _ -> ( (), Cmd.none )
        , subscriptions = \_ -> Sub.none
        }
