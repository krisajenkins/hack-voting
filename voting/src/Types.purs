module Types where

import Firebase as Firebase
import Control.Monad.Eff.Exception (Error)
import Data.Map (Map)
import Event.Types (EventId, EventMsg, EventState)
import Network.RemoteData (RemoteData)
import Routes (View)

------------------------------------------------------------

data Query a
    = UpdateView View a
    | Authenticate a
    | AuthResponse (RemoteData Error Firebase.User) a
    | EventMsg EventId EventMsg a

data Message
  = WatchEvent EventId
  | SignInAnonymously

type State =
    { view :: View
    , locationHost :: String
    , auth :: RemoteData Error Firebase.User
    , events :: Map EventId EventState
    , app :: Firebase.App
    }
