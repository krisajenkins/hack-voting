module Lenses where

import Control.Monad.Eff.Exception (Error)
import Data.Lens (Lens', Traversal', lens')
import Data.Lens.Index (ix)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))
import Event.Lenses (_first, _second, _third, _votes)
import Event.Types (Event, EventId, OptionId, Priority(Third, Second, First), Vote)
import Firebase (UID)
import Network.RemoteData (_success)
import Prelude ((<<<))
import Types

_events :: forall a b. Lens' {events :: b | a} b
_events = lens' (\record -> Tuple record.events (\events -> record {events = events}))
-- TODO In PureScript 0.11, I believe we could replace this with:
-- _events = prop (SProxy :: SProxy "events")

_event :: forall a b. Lens' {event :: b | a} b
_event = lens' (\record -> Tuple record.event (\event -> record {event = event}))

_auth :: forall a b. Lens' {auth :: b | a} b
_auth = lens' (\record -> Tuple record.auth (\auth -> record {auth = auth}))

_uid :: Lens' SomeUser UID
_uid = lens' (\(SomeUser someUser) -> Tuple someUser.uid (\uid -> SomeUser (someUser {uid = uid})))


_voteError :: Lens' EventState (Maybe Error)
_voteError = lens' (\eventState -> Tuple eventState.voteError (\voteError -> eventState {voteError = voteError}))

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

toLens :: Priority -> Lens' Vote (Maybe OptionId)
toLens First = _first
toLens Second = _second
toLens Third = _third
