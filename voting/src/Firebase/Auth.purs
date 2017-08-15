module Firebase.Auth
       ( getAuthService
       , signInAnonymously
       , signInWithPopup
       , makeGithubAuthProvider
       , GithubAuthProvider
       , LoginType(..)
       , UserCredential
       , class Provider
       , class HasPopupAuthFlow
       , AuthService
       , UID(..)
       , Email(..)
       , User(..)
       , _uid
       , _displayName
       , _email
       , _user
       )
       where

-- TODO Handle this error more accurately:
-- Q {code: "auth/operation-not-allowed", message: "The given sign-in provider is disabled for this Fi…under the sign-in method tab of the Auth section."}

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (Error)
import Data.Either (Either)
import Data.Foreign (Foreign)
import Data.Foreign.Class (class Decode)
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
import Firebase.Promise (Promise, decodePromise)

foreign import data AuthService :: Type

foreign import getAuthService :: App -> AuthService

newtype Credential = Credential
  { provider :: String
  , accessToken :: String
  }

derive instance genericCredential :: Generic Credential _
derive instance newtypeCredential :: Newtype Credential _

instance decodeCredential :: Decode Credential where
  decode = genericDecode (defaultOptions { unwrapSingleConstructors = true })

------------------------------------------------------------

foreign import signInAnonymously_ ::
  forall eff.
  AuthService
  -> Eff (firebase :: FIREBASE | eff) (Promise Foreign)

signInAnonymously ::
  forall eff.
  AuthService -> Aff (firebase :: FIREBASE | eff) (Either Error User)
signInAnonymously authService = do
  promise <- liftEff $ signInAnonymously_ authService
  decodePromise promise

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
derive instance ordEmail :: Ord Email
derive instance eqEmail :: Eq Email

instance showEmail :: Show Email where
  show = genericShow

instance decodeEmail :: Decode Email where
  decode = genericDecode (defaultOptions { unwrapSingleConstructors = true })

newtype User = User
  { uid :: UID
  , email :: NullOrUndefined Email
  , displayName :: NullOrUndefined String
  }

derive instance genericUser :: Generic User _
derive instance newtypeUser :: Newtype User _
derive instance eqUser :: Eq User

instance showUser :: Show User where
  show = genericShow

instance decodeUser :: Decode User where
  decode = genericDecode (defaultOptions { unwrapSingleConstructors = true })

_displayName :: Lens' User (Maybe String)
_displayName = _Newtype <<< Record.prop (SProxy :: SProxy "displayName") <<< _Newtype

_uid :: Lens' User UID
_uid = _Newtype <<< Record.prop (SProxy :: SProxy "uid")

_email :: Lens' User (Maybe Email)
_email = _Newtype <<< Record.prop (SProxy :: SProxy "email") <<< _Newtype

newtype UserCredential = UserCredential
  { user :: User
  , credential :: Credential
  }

derive instance genericUserCredential :: Generic UserCredential _
derive instance newtypeUserCredential :: Newtype UserCredential _

_user :: Lens' UserCredential User
_user = _Newtype <<< Record.prop (SProxy :: SProxy "user")

instance decodeUserCredential :: Decode UserCredential where
  decode = genericDecode (defaultOptions { unwrapSingleConstructors = true })

------------------------------------------------------------

class Provider a
class HasPopupAuthFlow a

foreign import signInWithPopup_ ::
  forall provider eff.
  HasPopupAuthFlow provider
  => Provider provider
  => AuthService
  -> provider
  -> Eff (firebase :: FIREBASE | eff) (Promise Foreign)

signInWithPopup ::
  forall provider eff.
  Provider provider
  => HasPopupAuthFlow provider
  => AuthService
  -> provider
  -> Aff (firebase :: FIREBASE | eff) (Either Error UserCredential)
signInWithPopup authService provider = do
  promise <- liftEff $ signInWithPopup_ authService provider
  decodePromise promise

------------------------------------------------------------

foreign import data GithubAuthProvider :: Type

instance providerGithubAuthProvider :: Provider GithubAuthProvider
instance hasPopupAuthFlowGithubAuthProvider :: HasPopupAuthFlow GithubAuthProvider

foreign import makeGithubAuthProvider :: forall eff. Eff eff GithubAuthProvider

------------------------------------------------------------

data LoginType = Anonymous | Github
