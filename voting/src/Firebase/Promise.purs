module Firebase.Promise where

import Control.Bind (bind, pure)
import Control.Category ((<<<))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error, error)
import Data.Unit (Unit, unit)
import Firebase.Core (FIREBASE, FirebaseError)

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
  Promise a
  -> (a -> Eff (firebase :: FIREBASE | eff) Unit)
  -> Eff (firebase :: FIREBASE | eff) Unit

foreign import andCatch :: forall a eff.
  Promise a
  -> (FirebaseError -> Eff (firebase :: FIREBASE | eff) Unit)
  -> Eff (firebase :: FIREBASE | eff) Unit
