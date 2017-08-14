module Routes where

import Prelude

import Control.Alternative ((<|>))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Event.Types (EventId(..))
import Routing.Match (Match)
import Routing.Match.Class (lit, str)

data View
    = FrontPage
    | EventView EventId
    | NotFound String

derive instance viewEq :: Eq View
derive instance viewGeneric :: Generic View _

instance viewShow :: Show View where
  show = genericShow

routing :: Match View
routing =
    FrontPage <$ lit ""
    <|>
    EventView <$> (lit "event" *> (EventId <$> str))
    <|>
    NotFound <$> str

type Router view = view -> String

pathRouter :: Router View
pathRouter view =
  "#" <> route view
  where
    route FrontPage = ""
    route (EventView (EventId eventId)) = "event/" <> eventId
    route (NotFound _) = ""
