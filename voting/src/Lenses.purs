module Lenses where

import Prelude
import Data.Lens (Lens', Traversal')
import Data.Lens.Index (ix)
import Data.Lens.Record (prop)
import Data.Symbol (SProxy(..))
import Event.Types (Event, EventId, Vote, _event, _votes)
import Firebase (UID)
import Network.RemoteData (_Success)
import Types (State)

_events :: forall a b. Lens' {events :: b | a} b
_events = prop (SProxy :: SProxy "events")

_auth :: forall a b. Lens' {auth :: b | a} b
_auth = prop (SProxy :: SProxy "auth")

-- TODO I wander if these ought to be IndexedTraversals?
toEvent :: EventId -> Traversal' State Event
toEvent eventId =
  _events
  <<< ix eventId
  <<< _event
  <<< _Success

toVote :: EventId -> UID -> Traversal' State Vote
toVote eventId uid =
  toEvent eventId
  <<< _votes
  <<< ix uid
