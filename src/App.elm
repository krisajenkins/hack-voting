module App exposing (main)

{-| The main entry point for the app.

@docs main
-}

import Navigation
import Routing
import State
import View


{-| Run the application.
-}
main : Program Never
main =
    Navigation.program (Navigation.makeParser (.hash >> Routing.hashParser))
        { init = State.initialState
        , update = State.update
        , urlUpdate = State.urlUpdate
        , subscriptions = State.subscriptions
        , view = View.root
        }
