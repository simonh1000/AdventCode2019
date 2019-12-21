port module Day11.Q1 exposing (..)

import Basics.Extra exposing (flip)
import Dict exposing (Dict)
import Json.Encode as Encode exposing (Value)
import List as L
import Set exposing (Set)


port toJs : Value -> Cmd msg


init : String -> ( (), Cmd msg )
init flags =
    ( ()
    , let
        recipes =
            convert flags

        graphOrder =
            recipes |> getGraphOrder |> convertOrder

        --1,031,737 , tgt < 10,000,000
      in
      findMin recipes graphOrder 1750000
        |> Encode.int
        |> toJs
    )


type alias Recipe =
    { yield : Int
    , ingredients : List ( Int, String )
    , total : Int
    }


ore =
    1000000000000


findMin : Dict String Recipe -> List String -> Int -> Int
findMin recipes graphOrder fuelTotal =
    let
        _ =
            Debug.log "trying: " fuelTotal
    in
    case handler fuelTotal graphOrder recipes |> (\res -> res > ore) of
        True ->
            fuelTotal - 1

        _ ->
            findMin recipes graphOrder (fuelTotal + 1)


handler : Int -> List String -> Dict String Recipe -> Int
handler fuelTotal graphOrder recipes =
    reduce fuelTotal graphOrder recipes
        |> Dict.get "ORE"
        |> Maybe.map .total
        |> Maybe.withDefault 0



-- part 3 - reducing


reduce : Int -> List String -> Dict String Recipe -> Dict String Recipe
reduce fuelTotal strings dict =
    dict
        |> Dict.insert "ORE" (Recipe 0 [] 0)
        |> Dict.update "FUEL" (Maybe.map <| \r -> { r | total = fuelTotal })
        |> (\d -> L.foldl reduce1 d strings)


reduce1 : String -> Dict String Recipe -> Dict String Recipe
reduce1 tgt recipes =
    case Dict.get tgt recipes of
        Just { yield, ingredients, total } ->
            let
                quants =
                    ceiling <| toFloat total / toFloat yield
            in
            ingredients
                |> L.map (Tuple.mapFirst ((*) quants))
                |> L.foldl addIngredients (Dict.remove tgt recipes)

        Nothing ->
            recipes


addIngredients : ( Int, String ) -> Dict String Recipe -> Dict String Recipe
addIngredients ( int, string ) dict =
    case Dict.get string dict of
        Just recipe ->
            Dict.insert string { recipe | total = recipe.total + int } dict

        Nothing ->
            dict



-- part 2 - find order


convertOrder : Dict String Int -> List String
convertOrder dict =
    convertOrderInner dict
        |> Dict.toList
        |> L.sortBy Tuple.first
        |> L.foldl (Tuple.second >> (++)) []


convertOrderInner : Dict String Int -> Dict Int (List String)
convertOrderInner =
    let
        go : String -> Int -> Dict Int (List String) -> Dict Int (List String)
        go string int dict =
            dict
                |> Dict.get int
                |> Maybe.withDefault []
                |> (::) string
                |> flip (Dict.insert int) dict
    in
    Dict.foldl go Dict.empty


getGraphOrder : Dict String Recipe -> Dict String Int
getGraphOrder recipes =
    let
        evalItem : ( Int, String ) -> Dict String Int -> ( Int, Dict String Int )
        evalItem ( x, item ) acc =
            if item == "ORE" then
                ( 0, acc )

            else
                case Dict.get item acc of
                    Just v ->
                        ( v, acc )

                    Nothing ->
                        case Dict.get item recipes of
                            Just recipe ->
                                evalRecipe item recipe acc
                                    |> evalItem ( x, item )

                            _ ->
                                ( 0, acc )

        evalRecipe : String -> Recipe -> Dict String Int -> Dict String Int
        evalRecipe name recipe acc =
            case Dict.get name acc of
                Just v ->
                    -- we already know the rank of this item
                    acc

                Nothing ->
                    -- analyse the recipe then
                    recipe.ingredients
                        |> L.foldl
                            (\it ( vs, acc_ ) ->
                                evalItem it acc_
                                    |> Tuple.mapFirst (\v -> v :: vs)
                            )
                            ( [], acc )
                        |> Tuple.mapFirst (L.maximum >> Maybe.withDefault 0 >> (+) 1)
                        |> (\( sum, acc_ ) -> Dict.insert name sum acc_)
    in
    Dict.foldl evalRecipe Dict.empty recipes



-- part 1


convert : String -> Dict String Recipe
convert string =
    string
        |> String.split "\n"
        |> L.map convertLine
        |> Dict.fromList


convertLine : String -> ( String, Recipe )
convertLine str =
    case String.split " => " str of
        [ ings, yd ] ->
            let
                ( yield, nm ) =
                    splitItem yd

                ingredients =
                    ings
                        |> String.split ", "
                        |> L.map splitItem
            in
            ( nm, { yield = yield, ingredients = ingredients, total = 0 } )

        _ ->
            Debug.todo "convertLine"


splitItem str =
    case String.split " " str of
        [ num, nm ] ->
            ( String.toInt num |> Maybe.withDefault 0, nm )

        _ ->
            Debug.todo "splitItem"


main =
    Platform.worker
        { init = init
        , update = \_ _ -> ( (), Cmd.none )
        , subscriptions = \_ -> Sub.none
        }
