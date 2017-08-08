module Event.View
       ( root )
       where

import Bootstrap (alert, badge, btnGroup, classList, col, empty, listGroup, listGroupItem, pullRight, row, well)
import CSS (pct, width)
import Common.View (errorView)
import Data.Array (sortWith)
import Data.Either (Either(..))
import Data.Foldable (all, maximum)
import Data.Identity (Identity)
import Data.Int (toNumber)
import Data.Lens (view, viewOn)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..), snd)
import Event.State (initialVote)
import Event.Types (Event(..), EventMsg(..), EventState, Option(..), OptionId, Priority(..), Vote, priorities, tally, toLens)
import Firebase as Firebase
import Halogen.HTML (ClassName(..), HTML, button, div, div_, h2_, h3_, h4_, i_, p_, span_, sup_, text)
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (class_, classes)
import Network.RemoteData (RemoteData(..))
import Prelude hiding (div)
import Slamdown as Slamdown
import Text.Markdown.SlamDown (SlamDown)
import Text.Markdown.SlamDown.Parser (parseMd)

root :: forall p. Firebase.User -> EventState -> HTML p EventMsg
root user state =
  div_ [ maybe empty (errorView "There was a problem loading this event.") state.eventError
       , maybe empty (errorView "There was a problem saving your vote.") state.voteError
       , maybe empty (errorView "There was a problem saving your option.") state.optionError
       , eventView user state.event
       ]

eventView :: forall p. Firebase.User -> RemoteData String Event -> HTML p EventMsg
eventView _ (Failure err) = div [ class_ alert.danger ] [ text err ]
eventView _ Loading = h2_ [ i_ [ text "Waiting for event data..." ] ]
eventView _ NotAsked = h2_ [ text "Initialising." ]
eventView user (Success event) =
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
          (optionView userVote <$> Map.toUnfoldable event.options)
      ]

optionView :: forall p. Vote -> Tuple OptionId Option -> HTML p EventMsg
optionView userVote (Tuple optionId (Option option)) =
  div [ class_ listGroupItem ]
      [ div [ class_ pullRight ]
          [ voteButtons userVote optionId ]
      , h3_ [ text option.name ]
      , descriptionView (parseMd <$> option.description)
      ]

descriptionView ::
  forall p i. Maybe (Either String SlamDown) -> HTML p i
descriptionView Nothing = empty
descriptionView (Just (Left err)) = i_ [ text err ]
descriptionView (Just (Right markdown)) =
  div_ $ unwrap (Slamdown.render markdown :: forall p' i'. Identity (Array (HTML p' i')))

voteButtons :: forall p. Vote -> OptionId -> HTML p EventMsg
voteButtons userVote optionId =
  let
    ordButton priority =
        let
          active =
            case view (toLens priority) userVote of
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

        tallied :: Array (Tuple OptionId Int)
        tallied =
            tally event.votes
                # Map.toUnfoldable
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
priorityString = format <<< ordinal
  where
    ordinal First = Tuple "1" "st"
    ordinal Second = Tuple "2" "nd"
    ordinal Third = Tuple "3" "rd"
    format (Tuple n suffix) =
      span_ [ text n, sup_ [ text suffix ] ]

votingFeedback :: forall p i. Vote -> HTML p i
votingFeedback userVote =
  div [ class_ (ClassName "voting-feedback") ]
    [ if all isJust options
      then div [ class_ alert.info ]
               [ h4_ [ text "Thanks for voting!" ]
               , p_ [ text "You can change your votes at any time." ]
               ]
      else div [ class_ alert.warning ]
               [ h4_ [ text "Please use your remaining votes." ] ]
    ]
  where
    options :: Array (Maybe OptionId)
    options = getOption <$> priorities

    getOption :: Priority -> Maybe OptionId
    getOption priority = viewOn userVote $ toLens priority
