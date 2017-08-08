module Event.Types where

import Control.Monad.Eff.Exception (Error)
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson, jsonEmptyObject, (.?), (:=), (~>))
import Data.Argonaut.Decode.Combinators ((.??))
import Data.Either (Either)
import Data.Foldable (foldl)
import Data.Generic (class Generic, gShow)
import Data.Lens (Lens', lens', view)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (mempty)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Tuple (Tuple(..))
import Firebase (UID)
import Network.RemoteData (RemoteData(..))
import Prelude (class Eq, class Ord, class Show, bind, id, pure, ($), (+), (<$>), (<<<), (<>))
import Test.QuickCheck (class Arbitrary, arbitrary)
import Utils (getFieldNullable, keyMap)

newtype OptionId = OptionId String

derive instance genericOptionId :: Generic OptionId
derive instance eqOptionId :: Eq OptionId
derive instance ordOptionId :: Ord OptionId
derive instance newtypeOptionId :: Newtype OptionId _

instance showOptionId :: Show OptionId where
  show = gShow

instance arbitraryOptionId :: Arbitrary OptionId where
  arbitrary = OptionId <$> arbitrary

instance encodeJsonOptionId :: EncodeJson OptionId where
    encodeJson (OptionId optionId) = encodeJson optionId

instance decodeJsonOptionId :: DecodeJson OptionId where
    decodeJson json = wrap <$> decodeJson json

newtype Option = Option
    { owner :: Maybe UID
    , name :: String
    , description :: Maybe String
    }

derive instance genericOption :: Generic Option
derive instance eqOption :: Eq Option
derive instance ordOption :: Ord Option
derive instance newtypeOption :: Newtype Option _

instance showOption :: Show Option where
  show = gShow

instance decodeJsonOption :: DecodeJson Option where
    decodeJson json = do
      object <- decodeJson json
      name <- object .? "name"
      description <- object .?? "description"
      pure $ Option { owner: Nothing, name, description }

------------------------------------------------------------

newtype Vote = Vote
    { first :: Maybe OptionId
    , second :: Maybe OptionId
    , third :: Maybe OptionId
    }

derive instance eqVote :: Eq Vote
derive instance genericVote :: Generic Vote

instance showVote :: Show Vote where
    show = gShow

instance decodeJsonVote :: DecodeJson Vote where
    decodeJson json = do
      object <- decodeJson json
      first  <- getFieldNullable object "first"
      second <- getFieldNullable object "second"
      third  <- getFieldNullable object "third"
      pure $ Vote { first, second, third }

instance encodeJsonVote :: EncodeJson Vote where
    encodeJson (Vote vote) =
      "first" := vote.first
      ~> "second" := vote.second
      ~> "third" := vote.third
      ~> jsonEmptyObject

instance arbitraryVote :: Arbitrary Vote where
  arbitrary = do
    first <- arbitrary
    second <- arbitrary
    third <- arbitrary
    pure $ Vote {first, second, third}

------------------------------------------------------------

newtype EventId = EventId String

derive instance newtypeEventId :: Newtype EventId _
derive instance genericEventId :: Generic EventId
derive instance eqEventId :: Eq EventId
derive instance ordEventId :: Ord EventId

instance showEventId :: Show EventId where
  show = gShow

------------------------------------------------------------

newtype Event = Event
    { title :: String
    , options :: Map OptionId Option
    , votes :: Map UID Vote
    }

derive instance eqEvent :: Eq Event
derive instance newtypeEvent :: Newtype Event _

instance showEvent :: Show Event where
  show (Event event) = "(Event " <> event.title <> ")"

instance decodeJsonEvent :: DecodeJson Event where
    decodeJson json = do
      object <- decodeJson json
      title <- object .? "title"
      options <- keyMap <$> object .? "options"
      votes <- keyMap <$> object .? "votes"
      pure $ Event { title
                   , options
                   , votes
                   }

------------------------------------------------------------

data Priority
    = First
    | Second
    | Third

derive instance genericPriority :: Generic Priority
derive instance eqPriority :: Eq Priority
derive instance ordPriority :: Ord Priority

instance showPriority :: Show Priority where
  show = gShow

-- TODO Can we infer this from Bounded or Enum?
priorities :: Array Priority
priorities = [ First, Second, Third ]

------------------------------------------------------------

data EventMsg
    = VoteFor Priority (Maybe OptionId)
    | EventUpdated (Either Error Json)

type EventState =
    { id :: EventId
    , event :: RemoteData String Event
    , eventError :: Maybe Error
    , voteError :: Maybe Error
    , optionError :: Maybe Error
    }

bestTitle :: EventState -> String
bestTitle state =
    case state.event of
        Success (Event event) ->
            event.title

        _ ->
            unwrap state.id

------------------------------------------------------------

tally :: Map UID Vote -> Map OptionId Int
tally =
  let
      increment :: Maybe Int -> Maybe Int
      increment =
        Just <<< (+) 1 <<< maybe 0 id

      tallyByPriority :: Vote -> Map OptionId Int -> Priority -> Map OptionId Int
      tallyByPriority vote acc priority =
        case view (toLens priority) vote of
          Nothing -> acc
          Just optionId -> Map.alter increment optionId acc

      tallyAllPriorities :: Map OptionId Int -> Vote -> Map OptionId Int
      tallyAllPriorities acc vote =
        foldl (tallyByPriority vote)
            acc
            priorities
  in
    foldl tallyAllPriorities mempty

------------------------------------------------------------

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

toLens :: forall f. Priority -> Lens' Vote (Maybe OptionId)
toLens First = _first
toLens Second = _second
toLens Third = _third
