module Firebase.Promise
       ( Promise
       , runPromise
       , decodePromise
       )
       where

import Control.Bind (pure, bind, discard)
import Control.Category ((<<<))
import Control.Monad.Aff (Aff, attempt, makeAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Except (runExcept)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foreign (Foreign)
import Data.Foreign.Class (class Decode, decode)
import Data.Function (($))
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Show (show)
import Data.Unit (Unit, unit)
import Firebase.Core (FIREBASE)

runPromise ::
  forall a eff.
  Promise a -> Aff (firebase :: FIREBASE | eff) (Either Error a)
runPromise = attempt <<< makeAff <<< promiseHandler

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

decodePromise :: forall eff a. Decode a => Promise Foreign -> Aff (firebase :: FIREBASE | eff) (Either Error a)
decodePromise promise = do
  value <- runPromise promise
  pure $ case value of
    Left err -> Left err
    Right foreignValue -> lmap (error <<< show) ((runExcept <<< decode) foreignValue)
