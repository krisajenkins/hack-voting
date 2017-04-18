module Types where

import Control.Monad.Eff.Exception (Error)
import Data.Either (Either)
import Data.Generic (class Generic, gShow)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Event.Types (Event, EventId, EventState, OptionId, Priority)
import Firebase (Email, UID)
import Firebase as Firebase
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
    -- TODO HeardEvent and EventUpdated are logically the same thing.
    = HeardEvent (Either Error Event) a
    | Ignore a
    | EventError Error a
    | VoteFor Priority (Maybe OptionId) a
    | OptionError Error a

data Query a
    = UpdateView View a
    | Authenticate a
    | AuthResponse (RemoteData Error SomeUser) a
    | EventMsg EventId (EventMsg a)
    | EventUpdated EventId (Either Error Firebase.Snapshot) a

data Message
  = WatchEvent EventId

type State =
    { view :: View
    , auth :: RemoteData Error SomeUser
    , events :: Map EventId EventState
    , app :: Firebase.App
    }
