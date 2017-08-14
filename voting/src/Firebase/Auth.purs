module Firebase.Auth
       ( getAuth
       , signInAnonymously
       , Auth
       , UID
       , Email
       , User
       , _uid
       , _email
       )
       where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Except (runExcept)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foreign (Foreign)
import Data.Foreign.Class (class Decode, decode)
import Data.Foreign.Generic (defaultOptions, genericDecode)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record as Record
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Firebase.Core (App, FIREBASE)
import Firebase.Promise (Promise, runPromise)

foreign import data Auth :: Type

foreign import getAuth :: forall eff. App -> Auth

------------------------------------------------------------

foreign import signInAnonymously_ ::
  forall eff.
  Auth
  -> Eff (firebase :: FIREBASE | eff) (Promise Foreign)

signInAnonymously ::
  forall aff.
  App -> Aff (firebase :: FIREBASE | aff) (Either Error User)
signInAnonymously app = do
  promise <- liftEff $ do
    signInAnonymously_ (getAuth app)
  value <- runPromise promise
  pure $ case value of
    Left err -> Left err
    Right foreignValue -> lmap (error <<< show) ((runExcept <<< decode) foreignValue)

------------------------------------------------------------

newtype UID = UID String

derive instance genericUID :: Generic UID _
derive instance eqUID :: Eq UID
derive instance ordUID :: Ord UID
derive instance newtypeUID :: Newtype UID _

instance showUID :: Show UID where
  show = genericShow

instance decodeUID :: Decode UID where
  decode = genericDecode (defaultOptions { unwrapSingleConstructors = true })

newtype Email = Email String

derive instance genericEmail :: Generic Email _
derive instance newtypeEmail :: Newtype Email _

instance showEmail :: Show Email where
  show = genericShow

instance decodeEmail :: Decode Email where
  decode = genericDecode (defaultOptions { unwrapSingleConstructors = true })

newtype User = User
  { uid :: UID
  , email :: NullOrUndefined Email
  }

derive instance genericUser :: Generic User _
derive instance newtypeUser :: Newtype User _

instance decodeUser :: Decode User where
  decode = genericDecode (defaultOptions { unwrapSingleConstructors = true })

_uid :: forall r a. Lens' User UID
_uid = _Newtype <<< Record.prop (SProxy :: SProxy "uid")

_email :: forall r a. Lens' User (Maybe Email)
_email = _Newtype <<< Record.prop (SProxy :: SProxy "email") <<< _Newtype
