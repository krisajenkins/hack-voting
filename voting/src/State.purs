module State where

import Types
import Data.Map as Map
import Firebase as Firebase
import Network.RemoteData as RemoteData
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.State (get, modify)
import Data.Argonaut (decodeJson, encodeJson)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Lens (assign, modifying, preview, set)
import Data.Lens.At (at)
import Data.Lens.Index (ix)
import Data.Map (Map)
import Data.Maybe (Maybe(..), fromMaybe)
import Event.Lenses (_event, _voteError, _votes, toLens)
import Event.State (initEventState, initialVote)
import Event.Types (EventId(..), EventMsg(..), EventState, OptionId, Priority, Vote)
import Firebase (App, Db, DbRef, FIREBASE, UID(..), getDbRef, getDbRefChild)
import Halogen (ComponentDSL, liftAff, raise)
import Lenses (_auth, _events, _uid, toEvent)
import Network.RemoteData (RemoteData(..), _success)
import Prelude (type (~>), bind, pure, show, ($), (<<<), (<>), (>>=), (>>>))
import Routes (View(..))

init :: App -> State
init app =
    { auth: NotAsked
    , events: Map.empty
    , view: FrontPage
    , app: app
    }

eval :: forall eff. Query ~> ComponentDSL State Query Message (Aff (console :: CONSOLE, firebase :: FIREBASE | eff))
eval (UpdateView view next) = do
  modify (_ { view = view })
  pure next
eval (AuthResponse response next) = do
  assign _auth response
  modifying _events (authEvents response)
  raise $ WatchEvent $ EventId "projects"
  raise $ WatchEvent $ EventId "languages"
  pure next
eval (Authenticate next) = pure next
eval (EventMsg eventId (VoteFor priority option) next) = do
  liftEff $ log $ "Got a vote: " <> show priority <> " - " <> show option
  state <- get
  -- TODO Refactor. This is a mess!
  case preview (_auth <<< _success <<< _uid) state of
    Nothing -> do
      liftEff $ log $ "No user, no vote."

    Just uid -> do
      liftEff $ log $ "User: " <> show uid
      modifying (toEvent eventId <<< _votes <<< at uid)
        (setVote priority option)
      let votePath = toEvent eventId <<< _votes <<< ix uid
      let voteErrorPath = _events <<< ix eventId <<< _voteError

      assign voteErrorPath Nothing

      newState <- get
      let vote :: Maybe Vote
          vote = preview votePath newState

      firebaseDb <- liftEff $ Firebase.getDb state.app
      let firebaseRef = voteDbRef eventId uid firebaseDb
      result <- liftAff $ Firebase.set firebaseRef (encodeJson vote)
      assign voteErrorPath $ case result of
        Left err -> Just err
        Right _ -> Nothing
  pure next

eval (EventMsg eventId (EventUpdated response) next) = do
  assign (_events <<< ix eventId <<< _event)
    (RemoteData.fromEither (lmap show response >>= decodeJson))
  pure next

voteDbRef :: EventId -> UID -> Db -> DbRef
voteDbRef (EventId eventId) (UID uid) =
  getDbRef "events"
  >>> getDbRefChild eventId
  >>> getDbRefChild "votes"
  >>> getDbRefChild uid

setVote :: Priority -> Maybe OptionId -> Maybe Vote -> Maybe Vote
setVote priority option =
  Just <<< set (toLens priority) option <<< fromMaybe initialVote

authEvents ::
  RemoteData Error SomeUser
  -> Map EventId EventState
  -> Map EventId EventState
authEvents (Success user) events = do
  foldl create events
    [ EventId "languages"
    , EventId "projects"
    ]
  where
    create :: Map EventId EventState -> EventId -> Map EventId EventState
    create map eventId = Map.insert eventId (initEventState eventId) map
authEvents _ m = m
