module View where

import Bootstrap (alert, btn, col, container, navTags, pullRight, row)
import Control.Monad.Eff.Exception (Error)
import Data.Array (fromFoldable)
import Data.Lens (preview, view)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String as String
import Event.Types (EventId, EventState, bestTitle)
import Event.View as Event
import Firebase (_displayName, _uid)
import Firebase as Firebase
import Halogen (ComponentHTML, action)
import Halogen.HTML (ClassName(ClassName), HTML, a, button, div, div_, h1, h2, h2_, h3_, h4_, header_, i_, li_, span_, text, ul)
import Halogen.HTML.Events (input_, onClick)
import Halogen.HTML.Properties (classes, disabled, href)
import Network.RemoteData (RemoteData(..))
import Prelude hiding (div)
import Routes (Router, View(..))
import Types (Query(..), State)

render :: Router View -> State -> ComponentHTML Query
render router state =
  div_
    [ header_
          [ container
              [ h1 [ classes [ ClassName "hostname" ] ]
                [ text state.locationHost ]
              , row
                  [ div [ classes [ col.xs12, col.sm8 ] ]
                      [ h3_ [ text "Vote-o-Matic" ] ]
                  , div [ classes [ col.xs12, col.sm4 ] ]
                      [ loginOutButtons state.auth ]
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

loginOutButtons :: forall p. RemoteData Error Firebase.User -> HTML p (Query Unit)
loginOutButtons auth =
  case auth of
    Success user -> h3_ [ text "Logged in as: "
                        , text (displayUser user)
                        ]
    Failure _ -> span_ [] -- The error is displayed elsewhere.
    NotAsked -> loginButtons false
    Loading -> loginButtons true

loginButtons :: forall p. Boolean -> HTML p (Query Unit)
loginButtons buttonsDisabled =
  row
    [ div [ classes [ col.xs6 ] ]
        [ button
            [ classes [ btn.primary, btn.block ]
            , onClick $ input_ (Authenticate Firebase.Anonymous)
            , disabled buttonsDisabled
            ]
            [ text "Login : Anonymous" ]
        ]
    , div [ classes [ col.xs6 ] ]
        [ button
            [ classes [ btn.primary, btn.block ]
            , onClick $ input_ (Authenticate Firebase.Github)
            , disabled buttonsDisabled
            ]
            [ text "Login : Github" ]
        ]
    ]

cannotLogin :: RemoteData Error Firebase.User -> Boolean
cannotLogin Loading = true
cannotLogin (Success _) = true
cannotLogin (Failure _) = false
cannotLogin NotAsked = false

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

mainView :: Firebase.User -> Map EventId EventState -> View -> ComponentHTML Query
mainView _ _ FrontPage = h3_ [ text "Choose one of the tabs above to start voting." ]
mainView user events (EventView eventId) = eventView user $ Map.lookup eventId events
mainView _ _ (NotFound _) = notFoundView

eventView :: Firebase.User -> Maybe EventState -> ComponentHTML Query
eventView user (Just event) = (action <<< EventMsg event.id) <$> Event.root user event
eventView _ Nothing = notFoundView

------------------------------------------------------------

notFoundView :: forall p i. HTML p i
notFoundView =
  h2_ [ text "404 - Not Found" ]

displayUser :: Firebase.User -> String
displayUser user =
   case view _displayName user of
     Just name -> name
     Nothing -> (String.take 5 (view (_uid <<< _Newtype) user)) <> "..."
