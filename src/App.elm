module App exposing (main)

{-| The main entry point for the app.

@docs main
-}

import Navigation
import Routing
import State
import Types exposing (..)
import View


{-| Run the application.
-}
main : Program Never Model Msg
main =
    Navigation.program (Routing.pathParser >> UrlUpdate)
        { init = State.init << Routing.pathParser
        , update = State.update
        , subscriptions = State.subscriptions
        , view = View.root Routing.pathRouter
        }
