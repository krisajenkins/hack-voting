module Firebase
       ( module Firebase.Core
       , module Firebase.Auth
       , module Firebase.Database
       ) where

import Firebase.Auth (Auth, Email(..), UID(..), _uid, _email, User, getAuth, signInAnonymously, signInAnonymously_)
import Firebase.Core (App, Config, FIREBASE, initializeApp)
import Firebase.Database
