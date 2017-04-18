module Firebase.Promise
       ( Promise
       , runPromise
       )
       where

import Control.Bind (bind, pure)
import Control.Category ((<<<))
import Control.Monad.Aff (Aff, attempt, makeAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error)
import Data.Either (Either)
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Unit (Unit, unit)
import Firebase.Core (FIREBASE)

runPromise ::
  forall a eff.
  Promise a -> Aff (firebase :: FIREBASE | eff) (Either Error a)
runPromise = attempt <<< makeAff <<< promiseHandler

-- TODO We're throwing away the error here. :-(
promiseHandler :: forall eff a.
  Promise a
  -> (Error -> Eff (firebase :: FIREBASE | eff) Unit)
  -> (a -> Eff (firebase :: FIREBASE | eff) Unit)
  -> Eff (firebase :: FIREBASE | eff) Unit
promiseHandler promise onError onSuccess = do
  runFn2 andThen promise onSuccess
  runFn2 andCatch promise onError
  pure unit

foreign import data Promise :: Type -> Type

foreign import andThen ::
  forall a eff.
  Fn2
    (Promise a)
    (a -> Eff (firebase :: FIREBASE | eff) Unit)
    (Eff (firebase :: FIREBASE | eff) Unit)

foreign import andCatch ::
  forall a eff.
  Fn2
    (Promise a)
    (Error -> Eff (firebase :: FIREBASE | eff) Unit)
    (Eff (firebase :: FIREBASE | eff) Unit)
