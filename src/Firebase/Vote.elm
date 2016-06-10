port module Firebase.Vote
    exposing
        ( Vote
        , voteSend
        , voteSendError
        , votes
        , votesListen
        , votesSilence
        )

import Firebase.Auth exposing (..)
import Firebase.Common exposing (..)


type alias Vote =
    { counter : Int }


port voteSend : ( UID, Vote ) -> Cmd msg


port voteSendError : (Error -> msg) -> Sub msg


port votes : (List ( String, Vote ) -> msg) -> Sub msg


port votesListen : () -> Cmd msg


port votesSilence : () -> Cmd msg
