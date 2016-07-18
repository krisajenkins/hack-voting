port module Event.Ports exposing (..)

import Event.Types exposing (..)
import Firebase.Auth exposing (..)
import Firebase.Common exposing (..)


port event : (String -> msg) -> Sub msg


port eventError : (Error -> msg) -> Sub msg


port eventListen : () -> Cmd msg


port eventSilence : () -> Cmd msg



------------------------------------------------------------


port voteSend : ( UID, Vote ) -> Cmd msg


port voteSendError : (Error -> msg) -> Sub msg



------------------------------------------------------------


port optionSend : Option -> Cmd msg


port optionSendError : (Error -> msg) -> Sub msg
