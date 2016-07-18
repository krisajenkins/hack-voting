module Event.Types exposing (..)

import Dict exposing (Dict)
import Firebase.Auth as Firebase
import Firebase.Common as Firebase
import RemoteData exposing (..)


type alias OptionId =
    String


type alias Event =
    { title : String
    , options : Dict OptionId Option
    , votes : Dict String Vote
    }


type alias Option =
    { owner : Maybe Firebase.UID
    , name : String
    , description : String
    }


type alias Vote =
    { first : Maybe OptionId
    , second : Maybe OptionId
    , third : Maybe OptionId
    }


type Priority
    = First
    | Second
    | Third


priorities : List Priority
priorities =
    [ First
    , Second
    , Third
    ]


voteN : Vote -> Priority -> Maybe OptionId
voteN vote priority =
    case priority of
        First ->
            vote.first

        Second ->
            vote.second

        Third ->
            vote.third


type alias Model =
    { event : RemoteData String Event
    , eventError : Maybe Firebase.Error
    , voteError : Maybe Firebase.Error
    , optionError : Maybe Firebase.Error
    }


type Msg
    = HeardEvent (Result String Event)
    | EventError Firebase.Error
    | VoteFor Priority (Maybe OptionId)
    | VoteError Firebase.Error
    | OptionError Firebase.Error


type FormMsg
    = SetName
    | SetDescription
