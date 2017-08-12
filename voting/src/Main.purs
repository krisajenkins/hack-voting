module Main where

import Control.Coroutine (Consumer, Producer, connect, consumer, emit, runProcess)
import Control.Monad.Aff (Aff, forkAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION, Error)
import DOM (DOM)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Document as Document
import Event.Types (EventMsg(..))
import Firebase (App, FIREBASE, User, initializeApp, signInAnonymously)
import Firebase as Firebase
import Halogen (Component, action, component, lift, liftEff)
import Halogen.Aff (HalogenEffects, awaitBody, runHalogenAff)
import Halogen.HTML (HTML)
import Halogen.VDom.Driver (runUI)
import Network.RemoteData (RemoteData(..), fromEither)
import Network.RemoteData as RemoteData
import Prelude
import Routes (View, pathRouter, routing)
import Routing (matchesAff)
import State as State
import Types (Message(..), Query(..))
import Utils (taggedConsumer)
import View as View

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

firebaseAuthProducer :: forall eff.
  App
  -> Producer
       (RemoteData Error User)
       (Aff (firebase :: FIREBASE, console :: CONSOLE, exception :: EXCEPTION | eff)) Unit
firebaseAuthProducer firebaseApp = do
  emit Loading
  result :: RemoteData Error User <- lift $ fromEither <$> signInAnonymously firebaseApp
  emit result

firebaseAuthConsumer
  :: forall eff
   . (Query ~> Aff (HalogenEffects (firebase :: FIREBASE | eff)))
  -> Consumer
       (RemoteData Error User)
       (Aff (HalogenEffects (firebase :: FIREBASE | eff)))
       Unit
firebaseAuthConsumer driver =
  taggedConsumer (driver <<< action <<< AuthResponse)

------------------------------------------------------------

watchEventMessages :: forall a eff.
  App
  -> (Query ~> Aff (HalogenEffects (console :: CONSOLE, firebase :: FIREBASE | eff)))
  -> Message
  -> Aff (HalogenEffects (console :: CONSOLE, firebase :: FIREBASE | eff)) (Maybe a)
watchEventMessages firebaseApp driverQuery (WatchEvent eventId) = do
  firebaseDb <- liftEff $ Firebase.getDb firebaseApp
  let ref =
        firebaseDb
        # Firebase.getDbRef "events"
        # Firebase.getDbRefChild (unwrap eventId)
  canceller <- forkAff $ runProcess $
    connect (Firebase.onValue ref) (taggedConsumer tagger)
  pure Nothing
  where
    tagger = RemoteData.fromEither >>> EventUpdated >>> EventMsg eventId >>> action >>> driverQuery

watchEventMessages firebaseApp driverQuery SignInAnonymously = do
  canceller <- forkAff $ runProcess $
    connect (firebaseAuthProducer firebaseApp) (firebaseAuthConsumer driverQuery)
  pure Nothing

------------------------------------------------------------
root :: forall aff.
  App
  -> String
  -> Component HTML Query Unit Message (Aff (firebase :: FIREBASE, dom :: DOM, console :: CONSOLE | aff))
root app locationHost = component
  { initialState: const (State.init app locationHost)
  , render: View.render pathRouter
  , eval: State.eval
  , receiver: const Nothing
  }

firebaseConfig :: Firebase.Config
firebaseConfig =
  { apiKey: "AIzaSyBG5-dI_sIjAC5KyQn5UEL9CLrhXwuiwgA"
  , authDomain: "voting-e6be5.firebaseapp.com"
  , databaseURL: "https://voting-e6be5.firebaseio.com"
  , storageBucket: ""
  }

main :: Eff (HalogenEffects (console :: CONSOLE, firebase :: FIREBASE)) Unit
main = runHalogenAff do
  body <- awaitBody
  firebaseApp <- liftEff $ initializeApp firebaseConfig
  locationHost <- liftEff Document.locationHost
  driver <- runUI (root firebaseApp locationHost) unit body

  -- This could be an initial message from the app.
  _ <- forkAff $ runProcess $ connect (firebaseAuthProducer firebaseApp) (firebaseAuthConsumer driver.query)

  _ <- forkAff $ driver.subscribe $ consumer $ watchEventMessages firebaseApp driver.query
  _ <- forkAff $ routeSignal driver.query

  pure unit
