module Firebase
       ( User
       , UID(..)
       , Email
       , FIREBASE
       , FirebaseError
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
       , signInAnonymously
       ) where

import Control.Coroutine (Producer)
import Control.Coroutine.Aff (produce)
import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION, Error, error)
import Data.Either (Either(..))
import Data.Foreign.Class (class IsForeign)
import Data.Generic (class Generic, gShow)
import Data.Maybe (Maybe)
import Data.Show (class Show)
import FFI.Util (property)
import Prelude (class Eq, Unit, bind, pure, unit, ($), (<$>), (<<<))

newtype UID = UID String

derive instance genericUID :: Generic UID
derive instance eqUID :: Eq UID

instance showUID :: Show UID where
  show = gShow

newtype Email = Email String

derive instance genericEmail :: Generic Email

instance showEmail :: Show Email where
  show = gShow

type FirebaseError = String

type Config =
  { apiKey :: String
  , authDomain :: String
  , databaseURL :: String
  , storageBucket :: String
  }

foreign import data FIREBASE :: !

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

foreign import getDbRef :: Db -> String -> DbRef
foreign import getDbRefChild :: DbRef -> String -> DbRef

foreign import on_ :: forall eff.
  DbRef
  -> String
  -> (Snapshot -> Eff (firebase :: FIREBASE | eff) Unit)
  -> (FirebaseError -> Eff (firebase :: FIREBASE | eff) Unit)
  -> Eff (firebase :: FIREBASE | eff) Unit

onValue :: forall eff.
  DbRef
  -> Producer (Either String Snapshot)
       (Aff (avar :: AVAR, firebase :: FIREBASE | eff)) Unit
onValue dbRef = produce \emit -> do
  on_ dbRef "value"
    (\snapshot -> emit $ Left $ Right snapshot)
    (\error -> emit $ Left $ Left error)

foreign import signInAnonymously_ :: forall eff. Auth -> Eff (firebase :: FIREBASE | eff) (Promise User)

signInAnonymously :: forall aff. App -> Aff (firebase :: FIREBASE | aff) User
signInAnonymously app = do
  auth <- liftEff $ getAuth app
  promise <- liftEff $ signInAnonymously_ auth
  makeAff $ promiseHandler promise

promiseHandler :: forall eff a.
  Promise a
  -> (Error -> Eff (firebase :: FIREBASE | eff) Unit)
  -> (a -> Eff (firebase :: FIREBASE | eff) Unit)
  -> Eff (firebase :: FIREBASE | eff) Unit
promiseHandler promise onError onSuccess = do
  andThen promise onSuccess
  andCatch promise (onError <<< error)
  pure unit

foreign import data Promise :: Type -> Type

foreign import andThen :: forall a eff.
  Promise a -> (a -> Eff (firebase :: FIREBASE | eff) Unit) -> Eff (firebase :: FIREBASE | eff) Unit

foreign import andCatch :: forall a eff.
  Promise a -> (FirebaseError -> Eff (firebase :: FIREBASE | eff) Unit) -> Eff (firebase :: FIREBASE | eff) Unit

uid :: User -> UID
uid user = UID $ property user "uid"

email :: User -> Maybe Email
email user = Email <$> property user "email"
