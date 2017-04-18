module Utils where

import Data.Map as Map
import Data.StrMap as StrMap
import Data.Argonaut (class DecodeJson, JObject, decodeJson)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Map (Map)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, wrap)
import Data.StrMap (StrMap)
import Data.Tuple (Tuple)
import Prelude (class Ord, pure, (<$>))

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