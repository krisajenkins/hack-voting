port module Firebase.Ports exposing (..)

import Exts.Maybe exposing (..)
import Exts.RemoteData exposing (..)
import Firebase.Types exposing (..)


port authenticate : () -> Cmd msg


port authError : (Error -> msg) -> Sub msg


port authStateChanged : (Maybe User -> msg) -> Sub msg


authResponse : Sub (RemoteData Error User)
authResponse =
    Sub.batch
        [ authError Failure
        , authStateChanged (maybe NotAsked Success)
        ]



------------------------------------------------------------


type alias Vote =
    { counter : Int }


port watch : () -> Cmd msg


port vote : ( User, Vote ) -> Cmd msg


port listenToVotes : (List ( String, Vote ) -> msg) -> Sub msg


port voteError : (Error -> msg) -> Sub msg
