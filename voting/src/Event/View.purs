module Event.View
       where

import Halogen.HTML
import Data.Map as Map
import Halogen.HTML.CSS as CSS
import Bootstrap (badge, empty, well, alert)
import CSS (pct, width)
import Data.Array (fromFoldable)
import Data.Foldable (maximum)
import Data.Int (toNumber)
import Data.Map (Map)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..), snd)
import Event.Types (Event(Event), Option, OptionId, Priority(Third, Second, First), Vote, priorities, tally, voteN)
import Halogen.HTML.Properties (class_)
import Prelude (id, negate, (#), ($), (*), (/), (<$>), (==), (>>>))
import Utils (sortWith)

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
