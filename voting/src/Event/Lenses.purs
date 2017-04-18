module Event.Lenses where

import Data.Lens
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))
import Event.Types (Event(..), OptionId, Vote(..))
import Firebase (UID)

_first :: Lens' Vote (Maybe OptionId)
_first = lens' (\(Vote record) -> Tuple record.first (\first -> Vote (record {first = first})))

_second :: Lens' Vote (Maybe OptionId)
_second = lens' (\(Vote record) -> Tuple record.second (\second -> Vote (record {second = second})))

_third :: Lens' Vote (Maybe OptionId)
_third = lens' (\(Vote record) -> Tuple record.third (\third -> Vote (record {third = third})))

_votes :: Lens' Event (Map UID Vote)
_votes = lens' (\(Event event) -> Tuple event.votes (\votes -> Event (event {votes = votes})))
