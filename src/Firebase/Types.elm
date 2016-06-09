module Firebase.Types exposing (..)


type alias Email =
    String


type alias Error =
    { code : String
    , message : String
    }


type alias User =
    { uid : String
    , email : Maybe Email
    , photoURL : Maybe String
    , emailVerified : Bool
    , displayName : Maybe String
    , isAnonymous : Bool
    }
