module Event.Rest exposing (..)

import Dict
import Event.Types exposing (..)
import Exts.Maybe
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode as Encode exposing (null)


encodeVote : Vote -> String
encodeVote vote =
    Encode.object
        [ ( "first", Exts.Maybe.maybe Encode.null Encode.string vote.first )
        , ( "second", Exts.Maybe.maybe Encode.null Encode.string vote.second )
        , ( "third", Exts.Maybe.maybe Encode.null Encode.string vote.third )
        ]
        |> Encode.encode 0


decodeVote : Decoder Vote
decodeVote =
    decode Vote
        |> optional "first" (maybe string) Nothing
        |> optional "second" (maybe string) Nothing
        |> optional "third" (maybe string) Nothing


decodeOption : Decoder Option
decodeOption =
    decode Option
        |> optional "owner" (maybe string) Nothing
        |> required "name" string
        |> required "description" string


decodeEvent : Decoder Event
decodeEvent =
    decode Event
        |> required "title" string
        |> optional "options" (dict decodeOption) Dict.empty
        |> optional "votes" (dict decodeVote) Dict.empty
