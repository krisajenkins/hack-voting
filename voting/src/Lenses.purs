module Lenses where

import Types
import Data.Lens (Lens', Traversal', lens')
import Data.Lens.Index (ix)
import Data.Tuple (Tuple(..))
import Event.Types (Event, EventId, Vote, _event, _votes)
import Firebase (UID)
import Network.RemoteData (_Success)
import Prelude ((<<<))

_events :: forall a b. Lens' {events :: b | a} b
_events = lens' (\record -> Tuple record.events (\events -> record {events = events}))
-- TODO In PureScript 0.11, I believe we could replace this with:
-- _events = prop (SProxy :: SProxy "events")

_auth :: forall a b. Lens' {auth :: b | a} b
_auth = lens' (\record -> Tuple record.auth (\auth -> record {auth = auth}))

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
