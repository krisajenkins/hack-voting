module Event.State where

import Data.Maybe (Maybe(..))
import Event.Types (EventId, State)
import Network.RemoteData (RemoteData(..))

init :: EventId -> State
init eventId =
  { id: eventId
  , event: Loading
  , eventError: Nothing
  , voteError: Nothing
  , optionError: Nothing
  }
