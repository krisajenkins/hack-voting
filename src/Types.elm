module Types exposing (..)

import Exts.RemoteData exposing (..)
import Firebase.Ports as Firebase
import Firebase.Types as Firebase


type Msg
    = Authenticate
    | AuthResponse (RemoteData Firebase.Error Firebase.User)
    | Increment
    | VoteError Firebase.Error
    | Heard (List ( String, Firebase.Vote ))


type alias Model =
    { auth : RemoteData Firebase.Error Firebase.User
    , counter : Int
    , votes : List ( String, Firebase.Vote )
    , voteError : Maybe Firebase.Error
    }
