module Types where

import Firebase as Firebase
import Control.Monad.Eff.Exception (Error)
import Data.Generic (class Generic, gShow)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Event.Types (EventId, EventMsg, EventState)
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

data Query a
    = UpdateView View a
    | Authenticate a
    | AuthResponse (RemoteData Error SomeUser) a
    | EventMsg EventId EventMsg a

data Message
  = WatchEvent EventId

type State =
    { view :: View
    , auth :: RemoteData Error SomeUser
    , events :: Map EventId EventState
    , app :: Firebase.App
    }
