module Types where

import Firebase as Firebase
import Control.Monad.Eff.Exception (Error)
import Data.Argonaut (Json)
import Data.Generic (class Generic, gShow)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Event.Types (EventId, EventState, OptionId, Priority)
import Firebase (Email, UID)
import Network.RemoteData (RemoteData)
import Prelude (class Show)
import Routes (View)

------------------------------------------------------------

newtype SomeUser = SomeUser
  { uid :: UID
  , email :: Maybe Email
  }

derive instance genericSomeUser :: Generic SomeUser

instance showSomeUser :: Show SomeUser where
  show = gShow

------------------------------------------------------------

data EventMsg a
    = VoteFor Priority (Maybe OptionId) a

data Query a
    = UpdateView View a
    | Authenticate a
    | AuthResponse (RemoteData Error SomeUser) a
    | EventMsg EventId (EventMsg a)
    | EventUpdated EventId (RemoteData Error Json) a

data Message
  = WatchEvent EventId

type State =
    { view :: View
    , auth :: RemoteData Error SomeUser
    , events :: Map EventId EventState
    , app :: Firebase.App
    }
