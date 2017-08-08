module Common.View where

import Halogen.HTML (HTML, code_, div, div_, h3_, text)
import Bootstrap (alert)
import Control.Monad.Eff.Exception (Error)
import Halogen.HTML.Properties (class_, classes)
import Prelude hiding (div)

errorView :: forall p i. String -> Error -> HTML p i
errorView title error =
    div [ class_ alert.danger ]
        [ h3_ [ text title ]
        , text $ show error
        ]

debuggingView :: forall a p i. Show a => String -> a -> HTML p i
debuggingView title thing =
  div [ classes [ alert.danger ] ]
    [ h3_ [ text title ]
    , div_ [ code_ [ text $ show thing ] ]
    ]
