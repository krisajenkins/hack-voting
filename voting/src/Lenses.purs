module Lenses where

import Types
import Data.Lens (Lens', Traversal', lens')
import Data.Lens.Index (ix)
import Data.Tuple (Tuple(..))
import Event.Lenses (_event, _votes)
import Event.Types (Event, EventId, Vote)
import Firebase (UID)
import Network.RemoteData (_success)
import Prelude ((<<<))

_events :: forall a b. Lens' {events :: b | a} b
_events = lens' (\record -> Tuple record.events (\events -> record {events = events}))
-- TODO In PureScript 0.11, I believe we could replace this with:
-- _events = prop (SProxy :: SProxy "events")

_auth :: forall a b. Lens' {auth :: b | a} b
_auth = lens' (\record -> Tuple record.auth (\auth -> record {auth = auth}))

_uid :: Lens' SomeUser UID
_uid = lens' (\(SomeUser someUser) -> Tuple someUser.uid (\uid -> SomeUser (someUser {uid = uid})))

toEvent :: EventId -> Traversal' State Event
toEvent eventId = _events <<< ix eventId <<< _event <<< _success

toVote :: EventId -> UID -> Traversal' State Vote
toVote eventId uid =
  _events
  <<< ix eventId
  <<< _event
  <<< _success
  <<< _votes
  <<< ix uid
