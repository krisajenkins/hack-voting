module View exposing (root)

import Dict exposing (Dict)
import Document
import Event.Types as Event exposing (EventId)
import Event.View
import Exts.Html.Bootstrap exposing (..)
import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import RemoteData exposing (..)
import Routing exposing (Router)
import Types exposing (..)


root : Router View -> Model -> Html Msg
root router model =
    div []
        [ header []
            [ container
                [ h1 [ class "hostname" ] [ text (Document.locationHost ()) ]
                , row
                    [ div [ class "col-xs-12 col-sm-9" ]
                        [ h3 [] [ text "Vote-o-Matic" ] ]
                    , div [ class "col-xs-12 col-sm-3" ]
                        [ button
                            [ class "btn btn-block btn-primary"
                            , onClick Authenticate
                            , disabled (canAuthenticate model)
                            ]
                            [ text "Log In" ]
                        ]
                    ]
                ]
            ]
        , container
            [ case model.auth of
                Success user ->
                    div []
                        [ eventLinks router model.events
                        , case model.view of
                            NotFound ->
                                h2 [] [ text "404 -Not Found" ]

                            FrontPage ->
                                h3 [] [ text "Choose one of the tabs above to start voting." ]

                            EventView eventId ->
                                case Dict.get eventId model.events of
                                    Nothing ->
                                        h2 [] [ text "404 -Not Found" ]

                                    Just eventModel ->
                                        Event.View.root user eventModel
                                            |> Html.map (EventMsg eventId)
                        ]

                Failure err ->
                    div [ class "alert alert-danger" ] [ text err.message ]

                Loading ->
                    h2 [] [ i [] [ text "Checking your account." ] ]

                NotAsked ->
                    h2 [ class "pull-right" ]
                        [ text "Log in to view and vote." ]
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


eventLinks : Router View -> Dict EventId Event.Model -> Html msg
eventLinks router events =
    ul [ class "nav nav-tabs" ]
        (events
            |> Dict.values
            |> List.map (eventLink router)
        )


eventLink : Router View -> Event.Model -> Html msg
eventLink router event =
    li []
        [ a [ href (router (EventView event.id)) ]
            [ h4 [] [ text <| Event.bestTitle event ] ]
        ]
