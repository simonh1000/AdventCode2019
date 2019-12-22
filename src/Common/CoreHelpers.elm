module Common.CoreHelpers exposing (..)

import Json.Decode as Decode exposing (Decoder)


ifThenElse : Bool -> a -> a -> a
ifThenElse cond yes no =
    if cond then
        yes

    else
        no


debugALittle : a -> a
debugALittle message =
    let
        _ =
            Debug.log "" (String.left 1000 <| Debug.toString message)
    in
    message


{-| e.g. debug just a part of a record in the middle of a pipeline
-}
debugWithFn : (a -> b) -> a -> a
debugWithFn function a =
    let
        _ =
            Debug.log "" <| function a
    in
    a


debugDecoder : Decoder a -> Decoder a
debugDecoder dec =
    Decode.value
        |> Decode.andThen
            (\val ->
                case Decode.decodeValue dec val of
                    Ok res ->
                        Decode.succeed res

                    Err err ->
                        Decode.fail <| Debug.log "debugDecoder" <| Decode.errorToString err
            )
