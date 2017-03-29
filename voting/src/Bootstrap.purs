module Bootstrap where

import Halogen.HTML (ClassName(ClassName), HTML, div, li, span, ul)
import Halogen.HTML.Properties (classes)

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
  , sm9: ClassName "col-sm-9"
  , md4:  ClassName "col-md-4"
  , md6:  ClassName "col-md-6"
  , md12: ClassName "col-md-12"
  }

pullRight :: ClassName
pullRight = ClassName "pull-right"

formControl :: ClassName
formControl = ClassName "form-control"

listGroup :: forall p i. Array (HTML p i) -> HTML p i
listGroup =
  ul [ classes [ ClassName "list-group" ] ]

listGroupItem :: forall p i. Array (HTML p i) -> HTML p i
listGroupItem =
  li [ classes [ ClassName "list-group-item" ] ]

labelInfo :: forall p i. Array (HTML p i) -> HTML p i
labelInfo =
  span [ classes [ ClassName "label"
                 , ClassName "label-info"] ]

alert :: { danger :: ClassName }
alert =
  { danger: ClassName "alert alert-danger" }

navTags :: ClassName
navTags = ClassName "nav nav-tabs"
