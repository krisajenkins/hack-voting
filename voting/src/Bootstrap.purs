module Bootstrap where

import Data.Array (filter)
import Data.Tuple (Tuple, fst, snd)
import Halogen.HTML (ClassName(ClassName), HTML, IProp, div, span, text)
import Halogen.HTML.Properties (classes)
import Prelude (map, ($))

container :: forall p i. Array (HTML p i) -> HTML p i
container =
  div [ classes [ ClassName "container" ] ]

row :: forall p i. Array (HTML p i) -> HTML p i
row =
  div [ classes [ ClassName "row" ] ]

-- TODO Flesh out.
btn ::
  { primary :: ClassName
  , success :: ClassName
  , block :: ClassName
  }
btn =
  { primary: ClassName "btn btn-primary"
  , success: ClassName "btn btn-success"
  , block: ClassName "btn btn-block"
  }

-- TODO Flesh out.
col =
  { xs4:  ClassName "col-xs-4"
  , xs6:  ClassName "col-xs-6"
  , xs12: ClassName "col-xs-12"
  , sm3: ClassName "col-sm-3"
  , sm6: ClassName "col-sm-6"
  , sm9: ClassName "col-sm-9"
  , md4:  ClassName "col-md-4"
  , md6:  ClassName "col-md-6"
  , md12: ClassName "col-md-12"
  }

btnGroup :: ClassName
btnGroup = ClassName "btn-group"

pullRight :: ClassName
pullRight = ClassName "pull-right"

formControl :: ClassName
formControl = ClassName "form-control"

listGroup :: ClassName
listGroup =
  ClassName "list-group"

listGroupItem :: ClassName
listGroupItem =
  ClassName "list-group-item"

labelInfo :: forall p i. Array (HTML p i) -> HTML p i
labelInfo =
  span [ classes [ ClassName "label"
                 , ClassName "label-info"] ]

alert ::
  { danger :: ClassName
  , info :: ClassName
  , warning :: ClassName
  }
alert =
  { danger: ClassName "alert alert-danger"
  , info: ClassName "alert alert-info"
  , warning: ClassName "alert alert-warning"
  }

navTags :: ClassName
navTags = ClassName "nav nav-tabs"

empty :: forall p i. HTML p i
empty = text ""

well :: forall p i. Array (HTML i p) -> HTML i p
well = div [ classes [ ClassName "well" ] ]

classList :: forall r i.
  Array (Tuple ClassName Boolean)
  -> IProp ("class" :: String | r) i
classList items =
  classes $ map fst $ filter snd items
