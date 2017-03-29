module TypesTests (all) where

import Prelude
import Data.Either (Either(..))
import Event.Types (EventId(..))
import Routing (match)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)
import Types (View(..), routing)

all :: forall aff. TestSuite aff
all =
  suite "Types" do
    test "Route parsing" do
      matches FrontPage ""
      matches (EventView (EventId "abc123")) "event/abc123"
      matches (NotFound "junk") "junk/junk"
  where
    matches view string =
      equal
        (Right view)
        (match routing string)
