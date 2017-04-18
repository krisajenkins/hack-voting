module Firebase.Auth where

import Firebase.Core (App, FIREBASE)
import Firebase.Auth (Auth, Email(..), UID(..), User, getAuth, signInAnonymously_)
import Firebase.Promise (Promise, runPromise)
import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (Error)
import Data.Either (Either)
import Data.Generic (class Generic, gShow)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import FFI.Util (property)

foreign import data Auth :: Type
foreign import data User :: Type

foreign import getAuth :: forall eff. App -> Eff (firebase :: FIREBASE | eff) Auth

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

uid :: User -> UID
uid user = UID $ property user "uid"

email :: User -> Maybe Email
email user = Email <$> property user "email"
