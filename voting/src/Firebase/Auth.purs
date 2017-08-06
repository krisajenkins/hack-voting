module Firebase.Auth where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (Error)
import Data.Bifunctor (rmap)
import Data.Either (Either)
import Data.Foreign (Foreign)
import Data.Foreign.Lens (prop, string)
import Data.Lens.Record as Record
import Data.Generic (class Generic, gShow)
import Data.Lens (view)
import Data.Lens.Types (Lens)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Firebase.Auth (Auth, Email(..), UID(..), User, getAuth, signInAnonymously_)
import Firebase.Core (App, FIREBASE)
import Firebase.Promise (Promise, runPromise)

foreign import data Auth :: Type

foreign import getAuth :: forall eff. App -> Eff (firebase :: FIREBASE | eff) Auth

foreign import signInAnonymously_ ::
  forall eff.
  Auth
  -> Eff (firebase :: FIREBASE | eff) (Promise Foreign)

type User =
  { uid :: UID
  , email :: Email
  }

_uid :: forall a b r. Lens { uid :: a | r } { uid :: b | r } a b
_uid = Record.prop (SProxy :: SProxy "uid")

_email :: forall a b r. Lens { email :: a | r } { email :: b | r } a b
_email = Record.prop (SProxy :: SProxy "email")

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

-- uid :: User -> UID
-- uid user = UID $ view (string <<< prop "uid") user

-- email :: User -> Maybe Email
-- email user = Email <$> view (string <<< prop "email") user
