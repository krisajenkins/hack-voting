module View where

import Event.Types
import Data.Map as Map
import Event.View as Event
import Bootstrap (alert, btn, col, container, navTags, pullRight, row)
import Control.Monad.Eff.Exception (Error)
import Data.Array (fromFoldable)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Halogen (ComponentHTML, action)
import Halogen.HTML (ClassName(ClassName), HTML, a, button, div, div_, h1, h2, h2_, h3_, h4_, header_, i_, li_, text, ul)
import Halogen.HTML.Events (input_, onClick)
import Halogen.HTML.Properties (classes, disabled, href)
import Network.RemoteData (RemoteData(..))
import Prelude (show, unit, ($), (<$>), (<<<))
import Routes (Router, View(..))
import Types (Query(..), SomeUser, State)

render :: Router View -> State -> ComponentHTML Query
render router state =
  div_
    [ header_
          [ container
              [ h1 [ classes [ ClassName "hostname" ] ]
                [ text "voting.clearercode.com" ] -- (Document.locationHost ()) ]
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
                  , mainView user state.events state.view
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

eventLinks :: forall p i. Router View -> Map EventId EventState -> HTML p i
eventLinks router events =
  ul [ classes [ navTags ] ]
   (fromFoldable (eventLink router <$> Map.values events))


eventLink :: forall p i. Router View -> EventState -> HTML p i
eventLink router eventState =
  li_
    [ a [ href (router (EventView eventState.id)) ]
        [ h4_ [ text $ bestTitle eventState ] ]
    ]

mainView :: SomeUser -> Map EventId EventState -> View -> ComponentHTML Query
mainView _ _ FrontPage = h3_ [ text "Choose one of the tabs above to start voting." ]
mainView user events (EventView eventId) = eventView user $ Map.lookup eventId events
mainView _ _ (NotFound _) = notFoundView

eventView :: SomeUser -> Maybe EventState -> ComponentHTML Query
eventView user (Just event) = (action <<< EventMsg event.id) <$> Event.root user event
eventView _ Nothing = notFoundView

------------------------------------------------------------

notFoundView :: forall p i. HTML p i
notFoundView =
  h2_ [ text "404 - Not Found" ]
