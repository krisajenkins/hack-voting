port module Event.Ports exposing (..)

import Event.Types exposing (..)
import Firebase.Auth exposing (..)
import Firebase.Common exposing (..)


port event : (( EventId, String ) -> msg) -> Sub msg


port eventError : (Error -> msg) -> Sub msg


port eventListen : EventId -> Cmd msg


port eventSilence : EventId -> Cmd msg



------------------------------------------------------------


port voteSend : ( EventId, UID, Vote ) -> Cmd msg


port voteSendError : (Error -> msg) -> Sub msg



------------------------------------------------------------


port optionSend : Option -> Cmd msg


port optionSendError : (Error -> msg) -> Sub msg
