module Types exposing (..)

import Dict exposing (Dict)
import Event.Types exposing (EventId)
import Firebase.Auth as Firebase
import Firebase.Common as Firebase
import RemoteData exposing (..)


type Msg
    = Authenticate
    | AuthResponse (RemoteData Firebase.Error Firebase.User)
    | EventMsg EventId Event.Types.Msg


type View
    = FrontPage
    | EventView EventId
    | NotFound


type alias Model =
    { auth : RemoteData Firebase.Error Firebase.User
    , events : Dict EventId Event.Types.Model
    , view : View
    }
