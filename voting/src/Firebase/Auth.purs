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
import Control.Monad.Eff.Exception (Error)
import Data.Bifunctor (rmap)
import Data.Either (Either)
import Data.Foreign (Foreign)
import Data.Foreign.Lens (prop, string)
import Data.Generic (class Generic, gShow)
import Data.Lens (Lens', view)
import Data.Lens.Record as Record
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Firebase.Core (App, FIREBASE)
import Firebase.Promise (Promise, runPromise)

foreign import data Auth :: Type

foreign import getAuth :: forall eff. App -> Eff (firebase :: FIREBASE | eff) Auth

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
    auth <- getAuth app
    signInAnonymously_ auth
  rmap asUser <$> runPromise promise
  where
    asUser :: Foreign -> User
    asUser = do
      uid <- UID <$> view (prop "uid" <<< string)
      email <- Email <$> view (prop "email" <<< string)
      pure { uid, email }

------------------------------------------------------------

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

type User =
  { uid :: UID
  , email :: Email
  }

_uid :: forall a r. Lens' { uid :: a | r } a
_uid = Record.prop (SProxy :: SProxy "uid")

_email :: forall a r. Lens' { email :: a | r } a
_email = Record.prop (SProxy :: SProxy "email")
