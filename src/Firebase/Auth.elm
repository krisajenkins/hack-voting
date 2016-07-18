port module Firebase.Auth
    exposing
        ( authenticate
        , authResponse
        , Email
        , UID
        , User
        )

import Exts.Maybe exposing (..)
import Firebase.Common exposing (..)
import RemoteData exposing (..)


type alias Email =
    String


type alias UID =
    String


type alias User =
    { uid : UID
    , email : Maybe Email
    , photoURL : Maybe String
    , emailVerified : Bool
    , displayName : Maybe String
    , isAnonymous : Bool
    }


port authenticate : () -> Cmd msg


port authError : (Error -> msg) -> Sub msg


port authStateChanged : (Maybe User -> msg) -> Sub msg


authResponse : Sub (RemoteData Error User)
authResponse =
    Sub.batch
        [ authError Failure
        , authStateChanged (maybe NotAsked Success)
        ]
