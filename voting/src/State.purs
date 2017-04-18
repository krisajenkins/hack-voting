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
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Lens (modifying, preview, set)
import Data.Lens.At (at)
import Data.Lens.Index (ix)
import Data.Map (Map)
import Data.Maybe (Maybe(..), fromMaybe)
import Firebase (App, Db, DbRef, FIREBASE, UID(..), getDbRef, getDbRefChild)
import Halogen (ComponentDSL, liftAff, raise)
import Lenses (_auth, _events, _uid, _voteError, _votes, toEvent, toLens)
import Network.RemoteData (RemoteData(..), _success)
import Prelude (type (~>), Unit, bind, const, pure, show, unit, ($), (<$>), (<<<), (<>), (>>>))

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
  modify $ \state -> state
             { auth = response
             , events = authEvents response state.events
             }
  raise $ WatchEvent $ EventId "projects"
  raise $ WatchEvent $ EventId "languages"
  pure next
eval (Authenticate next) = pure next
eval (EventMsg eventId (HeardEvent _ next)) = pure next
eval (EventMsg eventId (Ignore next)) = pure next
eval (EventMsg eventId (EventError _ next)) = pure next
eval (EventMsg eventId (VoteFor priority option next)) = do
  liftEff $ log $ "Got a vote: " <> show priority <> " - " <> show option
  state <- get
  -- TODO Refactor. This is a mess!
  case preview (_auth <<< _success <<< _uid) state of
    Nothing -> do
      liftEff $ log $ "No user, no vote."
    Just uid -> do
      liftEff $ log $ "User: " <> show uid
      modifying
        (toEvent eventId <<< _votes <<< at uid)
        (setVote priority option)
      modifying (_events <<< ix eventId <<< _voteError) (const Nothing)
      let path = toEvent eventId <<< _votes <<< ix uid
      newState <- get
      let vote :: Maybe Vote
          vote = preview path newState
      firebaseDb <- liftEff $ Firebase.getDb state.app
      let firebasePath = votePath eventId uid firebaseDb
      r :: Either Error Unit <- liftAff $ Firebase.set firebasePath (encodeJson vote)
      case r of
        Left err -> modifying (_events <<< ix eventId <<< _voteError) (const (Just err))
        Right _ -> liftEff $ log $ "GOT Success"
      pure unit
  pure next
eval (EventMsg eventId (OptionError _ next)) = pure next
eval (EventUpdated eventId (Left err) next) = do
  liftEff $ log $ "Got an error: " <> show err
  pure next
eval (EventUpdated eventId (Right snapshot) next) = do
  value <- liftEff $ do
             value <- decodeJson <$> Firebase.getVal snapshot
             log $ "Got a snapshot: " <> show value
             pure value
  modify (\state ->
           state { events = Map.update (_ { event = RemoteData.fromEither value } >>> Just)
                            eventId
                            state.events })
  pure next


votePath :: EventId -> UID -> Db -> DbRef
votePath (EventId eventId) (UID uid) =
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
