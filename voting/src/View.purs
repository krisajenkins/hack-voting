module View where

import Data.Map as Map
import Bootstrap (alert, btn, col, container, navTags, pullRight, row)
import Control.Monad.Eff.Exception (Error)
import Data.Array (fromFoldable)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Event.Types (EventId)
import Event.Types as Event
import Halogen (ComponentHTML)
import Halogen.HTML (ClassName(..), a, button, code_, div, div_, h1, h2, h2_, h3_, h4_, header_, i_, li_, ul, text)
import Halogen.HTML.Events (input_, onClick)
import Halogen.HTML.Properties (classes, disabled, href)
import Network.RemoteData (RemoteData(..))
import Prelude (class Show, show, ($), (<$>))
import Types (Query(Authenticate), SomeUser, State, View(..), Router)

render :: Router View -> State -> ComponentHTML Query
render router state =
    div_ [ header_
            [ container
                [ h1 [ classes [ ClassName "hostname" ] ] [ text "TODO HOSTNAME" ] -- (Document.locationHost ()) ]
                , row
                    [ div [ classes [ col.xs12, col.sm9 ] ]
                        [ h3_ [ text "Vote-o-Matic" ] ]
                    , div [ classes [ col.xs12, col.sm3 ] ]
                        [ button
                            [ classes [ btn.primary, btn.block ]
                            , onClick $ input_ Authenticate
                            , disabled (canAuthenticate state.auth)
                            ]
                            [ text "Log In" ]
                        ]
                    ]
                ]
            ]
        , container
            [ case state.auth of
                Success user ->
                    div_
                        [ eventLinks router state.events
                        , mainView state.events state.view
                        ]

                Failure err ->
                    div [ classes [ alert.danger ] ] [ text $ show err ]

                Loading ->
                    h2_ [ i_ [ text "Checking your account." ] ]

                NotAsked ->
                    h2 [ classes [ pullRight ] ]
                        [ text "Log in to view and vote." ]
            ]
        ]


canAuthenticate :: RemoteData Error SomeUser -> Boolean
canAuthenticate Loading = true
canAuthenticate (Success _) = true
canAuthenticate (Failure _) = false
canAuthenticate NotAsked = false


eventLinks :: forall query. Router View -> Map EventId Event.State -> ComponentHTML query
eventLinks router events =
    ul [ classes [ navTags ] ]
       (fromFoldable (eventLink router <$> Map.values events))


eventLink :: forall query. Router View -> Event.State -> ComponentHTML query
eventLink router event =
    li_
        [ a [ href (router (EventView event.id)) ]
            [ h4_ [ text $ Event.bestTitle event ] ]
        ]

mainView :: forall query. Map EventId Event.State -> View -> ComponentHTML query
mainView _ FrontPage = h3_ [ text "Choose one of the tabs above to start voting." ]
mainView events (EventView eventId) = eventView $ Map.lookup eventId events
mainView _ (NotFound _) = h2_ [ text "404 - Not Found" ]

eventView :: forall query. Maybe Event.State -> ComponentHTML query
eventView Nothing = h2 [] [ text "404 -Not Found" ]
eventView (Just event) = text "TODO"


debuggingView :: forall a query. Show a => String -> a -> ComponentHTML query
debuggingView title thing =
  div [ classes [ alert.danger ] ]
    [ h3_ [ text title ]
    , div_ [ code_ [ text $ show thing ] ]
    ]
