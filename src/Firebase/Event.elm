port module Firebase.Event exposing (..)

import Dict exposing (Dict)
import Firebase.Auth exposing (..)
import Firebase.Common exposing (..)


type alias ProjectId =
    String


type alias Event =
    { projects : Dict ProjectId Project
    , votes : Dict String Vote
    }


type alias Project =
    { name : String
    , description : String
    }


type alias Vote =
    { first : Maybe ProjectId
    , second : Maybe ProjectId
    , third : Maybe ProjectId
    }


initialVote : Vote
initialVote =
    { first = Nothing
    , second = Nothing
    , third = Nothing
    }



------------------------------------------------------------


port voteSend : ( UID, Vote ) -> Cmd msg


port voteSendError : (Error -> msg) -> Sub msg



------------------------------------------------------------


port projectSend : ( UID, Project ) -> Cmd msg


port projectSendError : (Error -> msg) -> Sub msg



------------------------------------------------------------


port event : (String -> msg) -> Sub msg


port eventError : (Error -> msg) -> Sub msg


port eventListen : () -> Cmd msg


port eventSilence : () -> Cmd msg
