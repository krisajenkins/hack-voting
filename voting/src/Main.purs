module Main where

import Prelude
import Firebase as Firebase
import State as State
import View as View
import Control.Coroutine (Consumer, Producer, connect, consumer, emit, runProcess)
import Control.Monad.Aff (Aff, forkAff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION, Error)
import DOM (DOM)
import Data.Argonaut (Json)
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Event.Types (EventMsg(..))
import Firebase (App, Db, FIREBASE, User, email, initializeApp, signInAnonymously, uid)
import Halogen (Component, action, component, lift, liftEff)
import Halogen.Aff (HalogenEffects, awaitBody, runHalogenAff)
import Halogen.HTML (HTML)
import Halogen.VDom.Driver (runUI)
import Network.RemoteData (RemoteData(..), fromEither)
import Routes (View, pathRouter, routing)
import Routing (matchesAff)
import Types (Message(..), Query(..), SomeUser(..))
import Utils (taggedConsumer)

routeSignal :: forall eff. (Query ~> Aff eff) -> Aff eff Unit
routeSignal driverQuery =
  matchesAff routing >>= redirects driverQuery

redirects :: forall eff.
  (Query ~> Aff eff)
  -> Tuple (Maybe View) View
  -> Aff eff Unit
redirects driverQuery (Tuple oldView newView) =
  driverQuery $ action $ UpdateView newView

------------------------------------------------------------

firebaseConfig :: Firebase.Config
firebaseConfig =
  { apiKey: "AIzaSyBG5-dI_sIjAC5KyQn5UEL9CLrhXwuiwgA"
  , authDomain: "voting-e6be5.firebaseapp.com"
  , databaseURL: "https://voting-e6be5.firebaseio.com"
  , storageBucket: ""
  }

firebaseAuthProducer :: forall eff.
  App
  -> Producer
       (RemoteData Error SomeUser)
       (Aff (firebase :: FIREBASE, console :: CONSOLE, err :: EXCEPTION | eff)) Unit
firebaseAuthProducer firebaseApp = do
  emit Loading
  result :: RemoteData Error User <- lift $ fromEither <$> signInAnonymously firebaseApp
  emit $ toSomeUser <$> result
  where
    toSomeUser user = SomeUser {uid: uid user, email: email user}

firebaseAuthConsumer
  :: forall eff
   . (Query ~> Aff (HalogenEffects (firebase :: FIREBASE | eff)))
  -> Consumer (RemoteData Error SomeUser)
       (Aff (HalogenEffects (firebase :: FIREBASE | eff))) Unit
firebaseAuthConsumer driver = consumer \someUser -> do
  driver $ action $ AuthResponse someUser
  pure Nothing

------------------------------------------------------------

watch :: forall a eff.
  Db
  -> (Query ~> Aff (avar :: AVAR, firebase :: FIREBASE, console :: CONSOLE | eff))
  -> Message
  -> Aff (avar :: AVAR, firebase :: FIREBASE, console :: CONSOLE | eff) (Maybe a)
watch firebaseDb driverQuery (WatchEvent eventId) = do
  forkAff $ refToMessageStream ref tagger
  pure Nothing
  where
    ref =
      firebaseDb
      # Firebase.getDbRef "events"
      # Firebase.getDbRefChild (unwrap eventId)
    tagger = EventUpdated >>> EventMsg eventId >>> action >>> driverQuery

refToMessageStream :: forall eff a.
  Firebase.DbRef
  -> (Either Error Json -> Aff (avar :: AVAR, firebase :: FIREBASE | eff) a)
  -> Aff (avar :: AVAR, firebase :: FIREBASE | eff) Unit
refToMessageStream ref tagger = do
  runProcess $ connect
    (Firebase.onValue ref)
    (taggedConsumer tagger)

------------------------------------------------------------

root :: forall aff.
  App
  -> Component HTML Query Unit Message (Aff (firebase :: FIREBASE, dom :: DOM, console :: CONSOLE | aff))
root app = component
  { initialState: const (State.init app)
  , render: View.render pathRouter
  , eval: State.eval
  , receiver: const Nothing
  }

------------------------------------------------------------

main :: Eff (HalogenEffects (console :: CONSOLE, firebase :: FIREBASE)) Unit
main = runHalogenAff do
  body <- awaitBody
  firebaseApp <- liftEff $ initializeApp firebaseConfig
  firebaseDb <- liftEff $ Firebase.getDb firebaseApp
  driver <- runUI (root firebaseApp) unit body

  forkAff $ runProcess $ connect (firebaseAuthProducer firebaseApp) (firebaseAuthConsumer driver.query)
  forkAff $ driver.subscribe $ consumer $ watch firebaseDb driver.query
  forkAff $ routeSignal driver.query

  pure unit
