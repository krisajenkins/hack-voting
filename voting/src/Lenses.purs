module Lenses where

import Types
import Data.Lens (Lens', Traversal', lens')
import Data.Lens.Index (ix)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))
import Firebase (UID)
import Network.RemoteData (_success)
import Prelude ((<<<))

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


_votes :: Lens' Event (Map UID Vote)
_votes = lens' (\(Event event) -> Tuple event.votes (\votes -> Event (event {votes = votes})))

_first :: Lens' Vote (Maybe OptionId)
_first = lens' (\(Vote record) -> Tuple record.first (\first -> Vote (record {first = first})))

_second :: Lens' Vote (Maybe OptionId)
_second = lens' (\(Vote record) -> Tuple record.second (\second -> Vote (record {second = second})))

_third :: Lens' Vote (Maybe OptionId)
_third = lens' (\(Vote record) -> Tuple record.third (\third -> Vote (record {third = third})))

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
