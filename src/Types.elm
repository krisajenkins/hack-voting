module Types exposing (..)

import Event.Types
import Firebase.Auth as Firebase
import Firebase.Common as Firebase
import RemoteData exposing (..)


type Msg
    = Authenticate
    | AuthResponse (RemoteData Firebase.Error Firebase.User)
    | EventMsg Event.Types.Msg


type alias Model =
    { auth : RemoteData Firebase.Error Firebase.User
    , eventModel : Maybe Event.Types.Model
    }
