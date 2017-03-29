module Event.Types where

import Data.Either (Either)
import Data.Generic (class Generic, gShow)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Firebase (UID, FirebaseError)
import Network.RemoteData (RemoteData(..))
import Prelude (class Eq, class Ord, class Show, show)

------------------------------------------------------------

newtype OptionId = OptionId String

derive instance genericOptionId :: Generic OptionId
derive instance eqOptionId :: Eq OptionId

instance showOptionId :: Show OptionId where
  show = gShow


newtype Option = Option
    { owner :: Maybe UID
    , name :: String
    , description :: Maybe String
    }

derive instance genericOption :: Generic Option
derive instance eqOption :: Eq Option

instance showOption :: Show Option where
  show = gShow


------------------------------------------------------------

newtype Vote = Vote
    { first :: Maybe OptionId
    , second :: Maybe OptionId
    , third :: Maybe OptionId
    }

derive instance eqVote :: Eq Vote

------------------------------------------------------------

newtype EventId = EventId String

derive instance genericEventId :: Generic EventId
derive instance eqEventId :: Eq EventId
derive instance ordEventId :: Ord EventId

instance showEventId :: Show EventId where
  show = gShow

newtype Event = Event
    { title :: String
    , options :: Map OptionId Option
    , votes :: Map String Vote
    }

derive instance eqEvent :: Eq Event

data Priority
    = First
    | Second
    | Third


priorities :: Array Priority
priorities =
    [ First
    , Second
    , Third
    ]


voteN :: Vote -> Priority -> Maybe OptionId
voteN (Vote vote) priority =
    case priority of
        First ->
            vote.first

        Second ->
            vote.second

        Third ->
            vote.third


type State =
    { id :: EventId
    , event :: RemoteData String Event
    , eventError :: Maybe FirebaseError
    , voteError :: Maybe FirebaseError
    , optionError :: Maybe FirebaseError
    }

data Msg
    = HeardEvent (Either String Event)
    | Ignore
    | EventError FirebaseError
    | VoteFor Priority (Maybe OptionId)
    | VoteError FirebaseError
    | OptionError FirebaseError

bestTitle :: State -> String
bestTitle state =
    case state.event of
        Success (Event event) ->
            event.title

        _ ->
            show state.id
