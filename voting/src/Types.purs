module Types where

import Routing.Match.Class
import Event.Types as Event
import Firebase as Firebase
import Control.Alt ((<|>))
import Control.Monad.Eff.Exception (Error)
import Data.Either (Either)
import Data.Generic (class Generic, gShow)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Event.Types (EventId(..))
import Firebase (Email, UID)
import Network.RemoteData (RemoteData)
import Prelude (class Eq, class Show, (*>), (<$), (<$>), (<>))
import Routing.Match (Match)

newtype SomeUser = SomeUser
  { uid :: UID
  , email :: Maybe Email
  }

derive instance genericSomeUser :: Generic SomeUser

instance showSomeUser :: Show SomeUser where
  show = gShow

data Query a
    = UpdateView View a
    | Authenticate a
    | AuthResponse (RemoteData Error SomeUser) a
    | SomeEvent (Either String Firebase.Snapshot) a
    | EventMsg EventId Event.Msg a

data Message
  = WatchEvent EventId

type State =
    { view :: View
    , auth :: RemoteData Error SomeUser
    , events :: Map EventId Event.State
    }


data View
    = FrontPage
    | EventView EventId
    | NotFound String

derive instance viewEq :: Eq View
derive instance viewGeneric :: Generic View

instance viewShow :: Show View where
  show = gShow

routing :: Match View
routing =
    (FrontPage <$ lit "")
    <|>
    (EventView <$> (lit "event" *> (EventId <$> str)))
    <|>
    (NotFound <$> str)

type Router view = view -> String

pathRouter :: Router View
pathRouter view =
    "#" <> route view
        where
          route FrontPage = ""
          route (EventView (EventId eventId)) = "event/" <> eventId
          route (NotFound _) = ""
