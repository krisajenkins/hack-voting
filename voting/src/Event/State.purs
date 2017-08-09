module Event.State where

import Event.Types (EventId, EventState, Vote(..))
import Data.Maybe (Maybe(..))
import Network.RemoteData (RemoteData(..))

initEventState :: EventId -> EventState
initEventState eventId =
  { id: eventId
  , event: Loading
    -- TODO Are these errors ever set?
  , eventError: Nothing
  , voteError: Nothing
  , optionError: Nothing
  }

initialVote :: Vote
initialVote = Vote
    { first: Nothing
    , second: Nothing
    , third: Nothing
    }
