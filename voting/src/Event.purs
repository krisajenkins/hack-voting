module Event where

import Bootstrap (alert, btnGroup, classList, col, empty, listGroup, listGroupItem, pullRight, row)
import Common.View (errorView)
import Data.Array (fromFoldable)
import Data.Either (Either(..))
import Data.Identity (Identity)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(Tuple))
import Event.Types (Event(Event), EventState, Option(Option), OptionId, Vote, initialVote, priorities, voteN)
import Event.View (priorityString, votesView, votingFeedback)
import Halogen (ClassName(..), ComponentHTML)
import Halogen.HTML (HTML, button, div, div_, h2_, h3_, i_, text)
import Halogen.HTML.Events (input_, onClick)
import Halogen.HTML.Properties (class_, classes)
import Network.RemoteData (RemoteData(..))
import Prelude (id, map, not, (#), ($), (<$>), (==))
import Slamdown as Slamdown
import Text.Markdown.SlamDown.Parser (parseMd)
import Types (EventMsg(..), Query(..), SomeUser(..))

render :: SomeUser -> EventState -> ComponentHTML Query
render user state =
  div_ [ maybe empty (errorView "There was a problem loading this event.") state.eventError
       , maybe empty (errorView "There was a problem saving your vote.") state.voteError
       , maybe empty (errorView "There was a problem saving your option.") state.optionError
       , case state.event of
           Success event ->
             EventMsg state.id <$> eventView user event

           Failure err ->
             div [ class_ alert.danger ] [ text err ]

           Loading ->
             h2_ [ i_ [ text "Waiting for event data..." ] ]

           NotAsked ->
             h2_ [ text "Initialising." ]
        ]

eventView :: SomeUser -> Event -> ComponentHTML EventMsg
eventView (SomeUser user) event =
  let
    userVote =
      Map.lookup user.uid (unwrap event).votes
        # maybe initialVote id
  in
    div_
      [ row
          [ div [ classes [ col.xs12, col.sm6 ] ]
              [ optionsView userVote event ]
          , div [ classes [ col.xs12, col.sm6 ] ]
              [ votesView event ]
          ]
      , row
          [ div [ classes [ col.xs12, col.sm6 ] ]
              [ votingFeedback userVote ]
          ]
      ]


optionsView :: Vote -> Event -> ComponentHTML EventMsg
optionsView userVote (Event event) =
  div_
      [ h2_ [ text event.title ]
      , div [ class_ listGroup ]
          (event.options
              # Map.toList
              # fromFoldable
              # map (optionView userVote)
          )
      ]

optionView :: Vote -> (Tuple OptionId Option) -> ComponentHTML EventMsg
optionView userVote (Tuple optionId (Option option)) =
  div [ class_ listGroupItem ]
      [ div [ class_ pullRight ]
          [ voteButtons userVote optionId ]
      , h3_ [ text option.name ]
      , case parseMd <$> option.description of
          Nothing -> empty
          Just (Left err) -> i_ [ text err ]
          Just (Right markdown) -> div_ $ unwrap (Slamdown.render markdown :: forall p i. Identity (Array (HTML p i)))
      ]

voteButtons :: Vote -> OptionId -> ComponentHTML EventMsg
voteButtons userVote optionId =
  let
    ordButton priority =
        let
          active =
            case voteN userVote priority of
              Nothing -> false
              Just votedOptionId -> votedOptionId == optionId
        in
          button
              [ classList
                  [ Tuple (ClassName "btn") true
                  , Tuple (ClassName "btn-default") (not active)
                  , Tuple (ClassName "btn-info") active
                  ]
              , onClick $ input_
                  (VoteFor priority
                      (if active then
                          Nothing
                       else
                          Just optionId
                      )
                  )
              ]
              [ priorityString priority ]
  in
    div [ class_ btnGroup ]
      (ordButton <$> priorities)
