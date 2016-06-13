module Types exposing (..)

import Exts.RemoteData exposing (..)
import Firebase.Auth as Firebase
import Firebase.Common as Firebase
import Firebase.Event as Firebase


type Msg
    = Authenticate
    | AuthResponse (RemoteData Firebase.Error Firebase.User)
    | ToggleListen
    | HeardEvent (Result String Firebase.Event)
    | EventError Firebase.Error
    | VoteError Firebase.Error
    | VoteFor Firebase.ProjectId Int


type alias Model =
    { auth : RemoteData Firebase.Error Firebase.User
    , listening : Bool
    , event : RemoteData String Firebase.Event
    , eventError : Maybe Firebase.Error
    , voteError : Maybe Firebase.Error
    }
