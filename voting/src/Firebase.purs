module Firebase
       ( User
       , UID(..)
       , Email
       , module Firebase.Core
       , Config
       , App
       , Db
       , DbRef
       , Snapshot
       , uid
       , email
       , initializeApp
       , getDb
       , getDbRef
       , getDbRefChild
       , onValue
       , set
       , getVal
       , signInAnonymously
       ) where

import Control.Coroutine (Producer)
import Control.Coroutine.Aff (produce)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION, Error)
import Data.Argonaut (Json)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn2, Fn4, runFn2, runFn4)
import Data.Generic (class Generic, gShow)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Show (class Show)
import FFI.Util (property)
import Firebase.Core (FIREBASE)
import Firebase.Promise (Promise, runPromise)
import Prelude (class Eq, class Ord, Unit, bind, ($), (<$>), (<<<))

newtype UID = UID String

derive instance genericUID :: Generic UID
derive instance eqUID :: Eq UID
derive instance ordUID :: Ord UID
derive instance newtypeUID :: Newtype UID _

instance showUID :: Show UID where
  show = gShow

newtype Email = Email String

derive instance genericEmail :: Generic Email

instance showEmail :: Show Email where
  show = gShow

type Config =
  { apiKey :: String
  , authDomain :: String
  , databaseURL :: String
  , storageBucket :: String
  }

foreign import data App :: Type
foreign import data User :: Type
foreign import data Db :: Type
foreign import data DbRef :: Type
foreign import data Auth :: Type
foreign import data Ref :: Type
foreign import data Snapshot :: Type

foreign import initializeApp :: forall eff. Config -> Eff (err :: EXCEPTION, firebase :: FIREBASE | eff) App

foreign import getAuth :: forall eff. App -> Eff (firebase :: FIREBASE | eff) Auth
foreign import getDb :: forall eff. App -> Eff (firebase :: FIREBASE | eff) Db

foreign import getDbRef_ :: Fn2 String Db DbRef
foreign import getDbRefChild_ :: Fn2 String DbRef DbRef

getDbRef :: String -> Db -> DbRef
getDbRef = runFn2 getDbRef_

getDbRefChild :: String -> DbRef -> DbRef
getDbRefChild = runFn2 getDbRefChild_

foreign import on_ ::
  forall eff.
  Fn4
    DbRef
    String
    (Snapshot -> Eff (firebase :: FIREBASE | eff) Unit)
    (Error -> Eff (firebase :: FIREBASE | eff) Unit)
    (Eff (firebase :: FIREBASE | eff) Unit)

-- TODO Snapshot should be Json - it's easier and still true.
-- TODO Make set into set_ and handle the promise internally, exposing an Aff.
foreign import set_ ::
  forall eff. Fn2 DbRef Json (Eff (firebase :: FIREBASE | eff) (Promise Unit))

set :: forall eff.
  DbRef
  -> Json
  -> Aff (firebase :: FIREBASE | eff) (Either Error Unit)
set dbRef json = do
  promise <- liftEff $ runFn2 set_ dbRef json
  runPromise promise

onValue :: forall eff.
  DbRef
  -> Producer
       (Either Error Snapshot)
       (Aff (avar :: AVAR, firebase :: FIREBASE | eff)) Unit
onValue dbRef = produce \emit -> do
  runFn4 on_ dbRef "value"
    (emit <<< Left <<< Right)
    (emit <<< Left <<< Left)

foreign import signInAnonymously_ ::
  forall eff.
  Auth
  -> Eff (firebase :: FIREBASE | eff) (Promise User)

signInAnonymously ::
  forall aff.
  App -> Aff (firebase :: FIREBASE | aff) (Either Error User)
signInAnonymously app = do
  promise <- liftEff $ do
    auth <- getAuth app
    signInAnonymously_ auth
  runPromise promise

uid :: User -> UID
uid user = UID $ property user "uid"

email :: User -> Maybe Email
email user = Email <$> property user "email"

foreign import getVal ::
  forall eff.
  Snapshot
  -> Eff (firebase :: FIREBASE | eff) Json
