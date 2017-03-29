module Response2 exposing (..)

import Exts.Tuple exposing (first, second)
import Monocle.Common exposing ((=>))
import Monocle.Optional exposing (..)


type alias Response model msg =
    ( model, Cmd msg )


type alias Update model msg =
    Response model msg -> Response model msg


fromModel : model -> Response model msg
fromModel model =
    ( model, Cmd.none )


mapModel : (submodel -> model) -> Response submodel msg -> Response model msg
mapModel =
    first


mapCmd : (submsg -> msg) -> Response model submsg -> Response model msg
mapCmd f =
    second (Cmd.map f)


{-| TODO I'm working on redoing the Response library with a mixture of:

* Using Monocle.
* Changing all update functions from `m -> (m,c)` to `(m,c) -> (m,c)`, for composability.

If we can get this funcition written, it's looking promising.
-}
mapOptional :
    Optional model submodel
    -> (Response submodel submsg -> Response submodel submsg)
    -> Response submodel submsg
    -> Response model submsg
mapOptional optional =
    modify (fromLens Monocle.Common.first => optional)


{-| Add a commmand into an existing `Response`.
-}
addCmd : Cmd msg -> Response model msg -> Response model msg
addCmd cmd1 =
    second (\cmd2 -> Cmd.batch [ cmd1, cmd2 ])
