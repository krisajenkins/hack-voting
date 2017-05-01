module Utils where

import Data.Map as Map
import Data.StrMap as StrMap
import Control.Category ((<<<))
import Control.Coroutine (Consumer, consumer)
import Control.Monad (class Monad)
import Control.Monad.State (class MonadState)
import Data.Argonaut (class DecodeJson, JObject, decodeJson)
import Data.Array (sortBy)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Lens (Lens, assign, use)
import Data.Map (Map)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, wrap)
import Data.Ord (comparing)
import Data.StrMap (StrMap)
import Data.Tuple (Tuple)
import Data.Unit (Unit)
import Prelude (class Ord, bind, pure, (<$>))

-- | Decodes:
-- |
-- | `{someKey: 5}` as `Just 5`
-- | `{someKey: null}` as `Nothing`
-- | `{}` as `Nothing`
getFieldNullable ::
  forall a. DecodeJson a =>
  JObject
  -> String
  -> Either String (Maybe a)
getFieldNullable o s =
  maybe
    (pure Nothing)
    decode
    (StrMap.lookup s o)
  where
    decode json = decodeJson json

-- | Convert a StrMap into a Map (Newtype ...)
keyMap :: forall k v.
  Ord k => Newtype k String => StrMap v -> Map k v
keyMap sm = asMap
  where
    arr :: Array (Tuple String v)
    arr = StrMap.toUnfoldable sm
    asMap = Map.fromFoldable (lmap wrap <$> arr)

-- | Sort an `Array` using an accessor function.
-- | TODO Builtin in Arrays v4.            -
sortWith :: forall a b. (Ord b) => (a -> b) -> Array a -> Array a
sortWith = sortBy <<< comparing

taggedConsumer ::
  forall r m i o.
  (Monad m) =>
  (i -> m o) -> Consumer i m r
taggedConsumer tagger =
  consumer \msg -> do
    tagger msg
    pure Nothing

assignM :: forall s a b m. MonadState s m => Lens s s a b -> m b -> m Unit
assignM l mv = do
  old <- use l
  new <- mv
  assign l new

modifyingM :: forall s a b m. MonadState s m => Lens s s a b -> (a -> m b) -> m Unit
modifyingM l mf = do
  old <- use l
  new <- mf old
  assign l new
