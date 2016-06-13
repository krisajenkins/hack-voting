module View exposing (root)

import Dict exposing (Dict)
import Exts.RemoteData exposing (..)
import Firebase.Auth exposing (User)
import Firebase.Event as Firebase exposing (Event, Project, Vote, ProjectId, initialVote)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Types exposing (..)


root : Model -> Html Msg
root model =
    div []
        [ button
            [ class "btn btn-primary"
            , onClick Authenticate
            , disabled (model.auth /= NotAsked)
            ]
            [ text "Log In" ]
        , case ( model.auth, model.event ) of
            ( Failure err, _ ) ->
                div [ class "alert alert-danger" ] [ text err.message ]

            ( _, Failure err ) ->
                div [ class "alert alert-danger" ] [ text err ]

            ( Success user, Success event ) ->
                eventView user event

            _ ->
                p [] [ text "Loading event." ]
        ]


eventView : User -> Event -> Html Msg
eventView user event =
    let
        userVote =
            Dict.get user.uid event.votes
                |> Maybe.withDefault initialVote
    in
        div [ class "row" ]
            [ div [ class "col-xs-12 col-sm-6" ]
                [ projectsView userVote event.projects ]
            , div [ class "col-xs-12 col-sm-6" ]
                [ votesView event.votes ]
            ]


projectsView : Vote -> Dict String Project -> Html Msg
projectsView userVote projects =
    div []
        [ h1 [] [ text "Projects" ]
        , div [ class "list-group" ]
            (projects
                |> Dict.toList
                |> List.map (projectView userVote)
            )
        ]


projectView : Vote -> ( String, Project ) -> Html Msg
projectView userVote ( id, project ) =
    div [ class "list-group-item" ]
        [ div [ class "pull-right" ]
            [ voteButtons userVote id ]
        , h3 [] [ text project.name ]
        , div [] [ text project.description ]
        ]


voteButtons : Vote -> ProjectId -> Html Msg
voteButtons userVote projectId =
    let
        ordButton n suffix accessor =
            let
                active =
                    case accessor userVote of
                        Nothing ->
                            False

                        Just votedProjectId ->
                            votedProjectId == projectId
            in
                button
                    [ classList
                        [ ( "btn", True )
                        , ( "btn-default", not active )
                        , ( "btn-info", active )
                        ]
                    , onClick (VoteFor projectId n)
                    ]
                    [ text (toString n)
                    , sup [] [ text suffix ]
                    ]
    in
        div [ class "btn-group" ]
            [ ordButton 1 "st" .first
            , ordButton 2 "nd" .second
            , ordButton 3 "rd" .third
            ]


votesView : Dict String Vote -> Html msg
votesView votes =
    div []
        [ h1 [] [ text "Votes" ]
        , div [ class "list-group" ]
            (votes
                |> Dict.toList
                |> List.map voteView
            )
        ]


voteView : ( String, Vote ) -> Html msg
voteView ( id, vote ) =
    div [ class "list-group-item" ]
        [ h3 [] [ text id ]
        , div [] [ code [] [ text (toString vote) ] ]
        ]
