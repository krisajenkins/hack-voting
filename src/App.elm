module App exposing (main)

{-| The main entry point for the app.

@docs main
-}

import Html.App
import State
import View


{-| Run the application.
-}
main : Program Never
main =
    Html.App.program
        { init = State.initialState
        , subscriptions = State.subscriptions
        , update = State.update
        , view = View.root
        }
