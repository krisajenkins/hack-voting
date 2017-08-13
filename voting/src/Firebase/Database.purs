module Firebase.Database
       ( Db
       , DbRef
       , getDb
       , class HasRef
       , getRef
       , onValue
       , set
       )
       where

import Firebase.Core (App, FIREBASE)
import Control.Bind (bind)
import Control.Coroutine (Producer)
import Control.Coroutine.Aff (produce)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (Error)
import Data.Argonaut (Json)
import Data.Either (Either(..))
import Data.Function (($), (<<<))
import Data.Function.Uncurried (Fn2, Fn4, runFn2, runFn4)
import Data.Unit (Unit)
import Firebase.Promise (Promise, runPromise)

foreign import data Db :: Type
foreign import data DbRef :: Type
foreign import data Ref :: Type
foreign import getDb :: forall eff. App -> Db

foreign import getDbRef_ :: Fn2 String Db DbRef
foreign import getDbRefChild_ :: Fn2 String DbRef DbRef

class HasRef a where
  getRef :: String -> a -> DbRef

instance hasRefDb :: HasRef Db where
  getRef = runFn2 getDbRef_

instance hasRefDbRef :: HasRef DbRef where
  getRef = runFn2 getDbRefChild_

foreign import on_ ::
  forall eff.
  Fn4
    DbRef
    String
    (Json -> Eff (firebase :: FIREBASE | eff) Unit)
    (Error -> Eff (firebase :: FIREBASE | eff) Unit)
    (Eff (firebase :: FIREBASE | eff) Unit)

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
       (Either Error Json)
       (Aff (avar :: AVAR, firebase :: FIREBASE | eff)) Unit
onValue dbRef = produce \emit -> do
  runFn4 on_ dbRef "value"
    (emit <<< Left <<< Right)
    (emit <<< Left <<< Left)
