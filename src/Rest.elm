module Rest exposing (..)

import Dict
import Exts.Maybe
import Firebase.Event exposing (..)
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


decodeProject : Decoder Project
decodeProject =
    decode Project
        |> required "name" string
        |> required "description" string


decodeEvent : Decoder Event
decodeEvent =
    decode Event
        |> optional "projects" (dict decodeProject) Dict.empty
        |> optional "votes" (dict decodeVote) Dict.empty
