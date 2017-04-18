module Firebase.Core
       ( FIREBASE
       , FirebaseError
       )
       where

type FirebaseError = String

foreign import data FIREBASE :: Effect
