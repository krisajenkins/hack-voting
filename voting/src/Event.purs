module Event where

import Text.Markdown.SlamDown.Syntax
import Data.Map as Map
import Slamdown as Slamdown
import Bootstrap (alert, btnGroup, classList, col, empty, listGroup, listGroupItem, pullRight, row, well)
import CSS.Geometry (width)
import CSS.Size (pct)
import Control.Monad.Eff.Exception (Error)
import Data.Array (fromFoldable, sortBy)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Identity (Identity)
import Data.Int (toNumber)
import Data.Map (Map)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (mempty)
import Data.Newtype (unwrap)
import Data.Traversable (maximum, traverse)
import Data.Tuple (Tuple(Tuple), snd)
import Firebase (UID)
import Halogen (ClassName(..), ComponentHTML)
import Halogen.HTML (HTML, button, div, div_, h2_, h3_, h4_, i_, p_, span, span_, sup_, text)
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Events (input_, onClick)
import Halogen.HTML.Properties (class_, classes)
import Network.RemoteData (RemoteData(..))
import Prelude (class Monad, class Ord, comparing, id, map, negate, not, show, (#), ($), (*), (+), (/), (<$>), (<<<), (==), (>>>))
import Text.Markdown.SlamDown.Parser (parseMd)
import Types (EventMsg(..), Query(..), SomeUser(..))
import Event.Types (Event(..), EventState, Option(..), OptionId, Priority(..), Vote, initialVote, priorities, voteN)

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


errorView :: forall query. String -> Error -> ComponentHTML query
errorView title error =
    div [ class_ alert.danger ]
        [ h3_ [ text title ]
        , text $ show error
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


votingFeedback :: forall query. Vote -> ComponentHTML query
votingFeedback userVote =
  div [ class_ (ClassName "voting-feedback") ]
    [ feedbackText  ( voteN userVote <$> priorities ) ]
  where
    feedbackText [Just _, Just _, Just _] =
      div [ class_ alert.info ]
          [ h4_ [ text "Thanks for voting!" ]
          , p_ [ text "You can change your votes at any time." ]
          ]
    feedbackText _ =
      div [ class_ alert.warning ]
          [ h4_ [ text "Please use your remaining votes." ] ]


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
          Just (Right markdown) -> div_ $ unwrap (slamDown markdown :: forall p i. Identity (Array (HTML p i)))
      ]

slamDown :: forall m p i. Monad m => SlamDown -> m (Array (HTML p i))
slamDown (SlamDown blocks) =
  fromFoldable <$> traverse Slamdown.renderBlock blocks

priorityString :: forall query. Priority -> ComponentHTML query
priorityString = format
  where
    format First = builder "1" "st"
    format Second = builder "2" "nd"
    format Third = builder "3" "rd"
    builder n suffix =
      span_ [ text n, sup_ [ text suffix ] ]


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
      ( ordButton <$> priorities )


tally :: Map UID Vote -> Map OptionId Int
tally votes =
  let
      increment :: Maybe Int -> Maybe Int
      increment =
        Just <<< (+) 1 <<< maybe 0 id

      tallyByPriority :: Vote -> Map OptionId Int -> Priority -> Map OptionId Int
      tallyByPriority vote acc priority =
        case voteN vote priority of
          Nothing -> acc
          Just optionId -> Map.alter increment optionId acc

      tallyAllPriorities :: Map OptionId Int -> Vote -> Map OptionId Int
      tallyAllPriorities acc vote =
        foldl (tallyByPriority vote)
            acc
            priorities
  in
    foldl tallyAllPriorities mempty votes


votesView :: forall query. Event -> ComponentHTML query
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

-- TODO Builtin in Arrays v4.            -
sortWith :: forall a b. (Ord b) => (a -> b) -> Array a -> Array a
sortWith = sortBy <<< comparing

voteBar ::
  forall query.
  Map OptionId Option
  -> Int
  -> Tuple OptionId Int
  -> ComponentHTML query
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


badge :: forall query. Int -> ComponentHTML query
badge n =
  span [ class_ $ ClassName "badge" ]
    [ text $ show n ]
