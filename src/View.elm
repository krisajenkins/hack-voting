module View exposing (root)

import Event.View
import Exts.Html.Bootstrap exposing (..)
import Exts.RemoteData exposing (..)
import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Types exposing (..)


root : Model -> Html Msg
root model =
    container
        [ button
            [ class "btn btn-primary"
            , onClick Authenticate
            , disabled (model.auth /= NotAsked)
            ]
            [ text "Log In" ]
        , case model.auth of
            Success user ->
                case model.eventModel of
                    Nothing ->
                        text "Initialising."

                    Just eventModel ->
                        Event.View.root user eventModel
                            |> Html.map EventMsg

            Failure err ->
                div [ class "alert alert-danger" ] [ text err.message ]

            Loading ->
                text "Loading."

            NotAsked ->
                text "Initialising."
        ]
