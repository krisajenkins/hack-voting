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
    div []
        [ container
            [ h1 [] [ text "Vote-o-matic" ]
            , button
                [ class "btn btn-primary pull-right"
                , onClick Authenticate
                , disabled (canAuthenticate model)
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
                    h2 [] [ i [] [ text "Checking your account." ] ]

                NotAsked ->
                    h2 [] [ text "Log in to view and vote." ]
            ]
        ]


canAuthenticate : Model -> Bool
canAuthenticate model =
    case model.auth of
        Loading ->
            True

        Success _ ->
            True

        NotAsked ->
            False

        Failure _ ->
            False
