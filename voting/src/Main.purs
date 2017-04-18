module Main where

import Control.Coroutine (Consumer, Producer, connect, consumer, emit, runProcess)
import Control.Monad.Aff (Aff, forkAff)
import Control.Monad.Aff.Console (logShow)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION, Error)
import DOM (DOM)
import Data.Argonaut (Json)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Firebase (App, FIREBASE, User, email, initializeApp, signInAnonymously, uid)
import Firebase as Firebase
import Halogen (Component, action, component, lift, liftEff)
import Halogen.Aff (HalogenEffects, awaitBody, runHalogenAff)
import Halogen.HTML (HTML)
import Halogen.VDom.Driver (runUI)
import Network.RemoteData (RemoteData(..), fromEither)
import Prelude
import Routes (View, pathRouter, routing)
import Routing (matchesAff)
import State as State
import Types (Message(..), Query(..), SomeUser(..))
import View as View

-- | TODO http://stackoverflow.com/questions/38370322/purescript-halogen-and-websockets
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

routeSignal :: forall eff. (Query Unit -> Aff eff Unit) -> Aff eff Unit
routeSignal driver = do
  Tuple old new <- matchesAff routing
  redirects driver old new

redirects :: forall eff.
  (Query Unit -> Aff eff Unit)
  -> Maybe View
  -> View
  -> Aff eff Unit
redirects driver _ view = do
  driver $ action $ UpdateView view

------------------------------------------------------------

firebaseMessageHandlerThing :: forall eff.
  App
  -> (Query ~> Aff (HalogenEffects (console :: CONSOLE, firebase :: FIREBASE | eff)))
  -> Consumer Message (Aff (HalogenEffects (console :: CONSOLE, firebase :: FIREBASE | eff))) Unit
firebaseMessageHandlerThing firebaseApp driver =
  consumer $ foo firebaseApp driver

foo :: forall eff a.
  App
  -> (Query ~> Aff (HalogenEffects (console :: CONSOLE, firebase :: FIREBASE | eff)))
  -> Message
  -> Aff (HalogenEffects (console :: CONSOLE, firebase :: FIREBASE | eff)) (Maybe a)
foo firebaseApp driver (WatchEvent eventId) = do
  firebaseDb <- liftEff $ Firebase.getDb firebaseApp
  let eventRef = firebaseDb
          # Firebase.getDbRef "events"
          # Firebase.getDbRefChild (unwrap eventId)
  forkAff $ runProcess $ connect (Firebase.onValue eventRef) $
      consumer \msg -> do
        processed <-
          case msg of
            Right snapshot -> do
              val :: Json <- liftEff $ Firebase.getVal snapshot
              pure (Success val)
            Left err -> do
              pure (Failure err)
        driver $ action $ EventUpdated eventId processed
        pure Nothing
  logShow eventId
  pure Nothing

main :: Eff (HalogenEffects (console :: CONSOLE, firebase :: FIREBASE)) Unit
main = runHalogenAff do
  body <- awaitBody
  firebaseApp <- liftEff $ initializeApp firebaseConfig
  driver <- runUI (root firebaseApp) unit body

  forkAff $ runProcess $ connect (firebaseAuthProducer firebaseApp) (firebaseAuthConsumer driver.query)
  forkAff $ driver.subscribe (firebaseMessageHandlerThing firebaseApp driver.query)
  forkAff $ routeSignal driver.query

  pure unit
