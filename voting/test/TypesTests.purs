module TypesTests (all) where

import Types
import Control.Monad.Eff.Random (RANDOM)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson)
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Either (Either(..))
import Data.Traversable (sequence_)
import Data.Tuple (Tuple(..))
import Prelude (class Eq, class Show, bind, show, ($), (<$>), (<>), (==))
import Routing (match)
import Test.QuickCheck (Result, (<?>))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)
import Test.Unit.QuickCheck (quickCheck)
import Type.Proxy (Proxy(..))

all :: forall aff. TestSuite (random :: RANDOM | aff)
all =
  suite "Types" do
    routeParsing
    jsonHandling

routeParsing :: forall aff. TestSuite aff
routeParsing = do
    test "Route parsing" do
      sequence_ $ matches <$>
        [ Tuple FrontPage ""
        , Tuple (EventView (EventId "abc123")) "event/abc123"
        , Tuple (NotFound "junk") "junk/junk"
        ]
  where
    matches (Tuple view string) =
      equal
        (Right view)
        (match routing string)

jsonHandling :: forall aff. TestSuite (random :: RANDOM | aff)
jsonHandling = do
    test "Json handling" do
      quickCheck $ serialise $ Proxy :: Proxy OptionId
      quickCheck $ serialise $ Proxy :: Proxy Vote
      equal
        (Right initialVote)
        (decodeJson jsonEmptyObject)

serialise ::
  forall a. (DecodeJson a, EncodeJson a, Eq a, Show a) =>
  Proxy a ->
  a -> Result
serialise _ a =
  case decodeJson $ encodeJson a of
    Left err -> false <?> err
    Right decoded -> a == decoded <?> ("x = " <> show a <> ", decoded = " <> show decoded)
