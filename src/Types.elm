module Types exposing (..)

import Exts.RemoteData exposing (..)
import Firebase.Auth as Firebase
import Firebase.Common as Firebase
import Firebase.Vote as Firebase


type Msg
    = Authenticate
    | AuthResponse (RemoteData Firebase.Error Firebase.User)
    | Increment
    | VoteError Firebase.Error
    | HeardVotes (List ( Firebase.UID, Firebase.Vote ))
    | ToggleListen


type alias Model =
    { auth : RemoteData Firebase.Error Firebase.User
    , counter : Int
    , listening : Bool
    , votes : List ( String, Firebase.Vote )
    , voteError : Maybe Firebase.Error
    }
