module Event.View
       ( root )
       where

import Bootstrap
import Event.Types (Event(..), EventMsg(..), EventState, Option(..), OptionId, Priority(..), Vote, initialVote, priorities, tally, voteN)
import Data.Map as Map
import Halogen.HTML.CSS as CSS
import Slamdown as Slamdown
import CSS (pct, width)
import Common.View (errorView)
import Data.Array (fromFoldable)
import Data.Either (Either(..))
import Data.Foldable (maximum)
import Data.Identity (Identity)
import Data.Int (toNumber)
import Data.Map (Map)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..), snd)
import Halogen.HTML (ClassName(..), HTML, button, div, div_, h2_, h3_, h4_, i_, p_, span_, sup_, text)
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (class_, classes)
import Network.RemoteData (RemoteData(..))
import Prelude (const, id, map, negate, not, (#), ($), (*), (/), (<$>), (==), (>>>))
import Text.Markdown.SlamDown.Parser (parseMd)
import Types (SomeUser(..))
import Utils (sortWith)

root :: forall p. SomeUser -> EventState -> HTML p EventMsg
root user state =
  div_ [ maybe empty (errorView "There was a problem loading this event.") state.eventError
       , maybe empty (errorView "There was a problem saving your vote.") state.voteError
       , maybe empty (errorView "There was a problem saving your option.") state.optionError
       , case state.event of
           Success event ->
             eventView user event

           Failure err ->
             div [ class_ alert.danger ] [ text err ]

           Loading ->
             h2_ [ i_ [ text "Waiting for event data..." ] ]

           NotAsked ->
             h2_ [ text "Initialising." ]
        ]

eventView :: forall p. SomeUser -> Event -> HTML p EventMsg
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


optionsView :: forall p. Vote -> Event -> HTML p EventMsg
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

optionView :: forall p. Vote -> Tuple OptionId Option -> HTML p EventMsg
optionView userVote (Tuple optionId (Option option)) =
  div [ class_ listGroupItem ]
      [ div [ class_ pullRight ]
          [ voteButtons userVote optionId ]
      , h3_ [ text option.name ]
      , case parseMd <$> option.description of
          Nothing -> empty
          Just (Left err) -> i_ [ text err ]
          Just (Right markdown) -> div_ $ unwrap (Slamdown.render markdown :: forall q i. Identity (Array (HTML q i)))
      ]

voteButtons :: forall p. Vote -> OptionId -> HTML p EventMsg
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
              , onClick $
                (const (Just (VoteFor priority
                         (if active then
                            Nothing
                          else
                            Just optionId
                         )
                        )))
              ]
              [ priorityString priority ]
  in
    div [ class_ btnGroup ]
      (ordButton <$> priorities)
votesView :: forall p i. Event -> HTML p i
votesView (Event event) =
    let
        voteCounts =
            tally event.votes

        maxCount =
            voteCounts
                # Map.values
                # maximum
                # maybe 0 id

        tallied =
            tally event.votes
                # Map.toList
                # fromFoldable
                # sortWith (snd >>> negate)
    in
        div_
            [ h2_ [ text "Live Results" ]
            , if tallied == [] then
                empty
              else
                well (voteBar event.options maxCount <$> tallied)
            ]
voteBar ::
  forall p i.
  Map OptionId Option
  -> Int
  -> Tuple OptionId Int
  -> HTML p i
voteBar options maxCount (Tuple optionId voteCount) =
    let
        name =
            case Map.lookup optionId options of
                Nothing ->
                    unwrap optionId

                Just option ->
                    _.name (unwrap option)

    in
        div [ class_ $ ClassName "vote" ]
            [ h3_
                [ text name
                , text " "
                , badge voteCount
                ]
            , div
                [ class_ $ ClassName "bar"
                , CSS.style do
                    width $ pct $ (toNumber voteCount / toNumber maxCount) * 100.0
                ]
                []
            ]

priorityString :: forall p i. Priority -> HTML p i
priorityString = format
  where
    format First = builder "1" "st"
    format Second = builder "2" "nd"
    format Third = builder "3" "rd"
    builder n suffix =
      span_ [ text n, sup_ [ text suffix ] ]

votingFeedback :: forall p i. Vote -> HTML p i
votingFeedback userVote =
  div [ class_ (ClassName "voting-feedback") ]
    [ feedbackText  (voteN userVote <$> priorities) ]
  where
    feedbackText [Just _, Just _, Just _] =
      div [ class_ alert.info ]
          [ h4_ [ text "Thanks for voting!" ]
          , p_ [ text "You can change your votes at any time." ]
          ]
    feedbackText _ =
      div [ class_ alert.warning ]
          [ h4_ [ text "Please use your remaining votes." ] ]
