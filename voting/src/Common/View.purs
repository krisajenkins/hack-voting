module Common.View where

import Halogen.HTML
import Bootstrap (alert)
import Control.Monad.Eff.Exception (Error)
import Halogen.HTML.Properties (class_, classes)
import Prelude (class Show, show, ($))

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
