module Event.Lenses where

import Data.Lens
import Event.Types (Event(..), EventState, OptionId, Priority(..), Vote(..))
import Control.Monad.Eff.Exception (Error)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))
import Firebase (UID)

_first :: Lens' Vote (Maybe OptionId)
_first = lens' (\(Vote record) -> Tuple record.first (\first -> Vote (record {first = first})))

_second :: Lens' Vote (Maybe OptionId)
_second = lens' (\(Vote record) -> Tuple record.second (\second -> Vote (record {second = second})))

_third :: Lens' Vote (Maybe OptionId)
_third = lens' (\(Vote record) -> Tuple record.third (\third -> Vote (record {third = third})))

_event :: forall a b. Lens' {event :: b | a} b
_event = lens' (\record -> Tuple record.event (\event -> record {event = event}))

_votes :: Lens' Event (Map UID Vote)
_votes = lens' (\(Event event) -> Tuple event.votes (\votes -> Event (event {votes = votes})))

_voteError :: Lens' EventState (Maybe Error)
_voteError = lens' (\eventState -> Tuple eventState.voteError (\voteError -> eventState {voteError = voteError}))

toLens :: Priority -> Lens' Vote (Maybe OptionId)
toLens First = _first
toLens Second = _second
toLens Third = _third
