module Event.Types exposing (..)

import Dict exposing (Dict)
import Exts.RemoteData exposing (..)
import Firebase.Common as Firebase


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


type alias Model =
    { event : RemoteData String Event
    , eventError : Maybe Firebase.Error
    , voteError : Maybe Firebase.Error
    }


type Msg
    = HeardEvent (Result String Event)
    | EventError Firebase.Error
    | VoteError Firebase.Error
    | VoteFor ProjectId Int
