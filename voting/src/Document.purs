module Document
       ( locationHost
       )
       where

import Control.Monad.Eff (Eff)

foreign import locationHost :: forall eff. Eff eff String
