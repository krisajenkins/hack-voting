module Event.View exposing (root)

import Dict exposing (Dict)
import Event.State exposing (..)
import Event.Types exposing (..)
import Exts.RemoteData exposing (..)
import Firebase.Auth exposing (User)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


root : User -> Model -> Html Msg
root user model =
    case model.event of
        Success event ->
            eventView user event

        Failure err ->
            div [ class "alert alert-danger" ] [ text err ]

        Loading ->
            p [] [ text "Waiting for event data..." ]

        NotAsked ->
            p [] [ text "Initialising." ]


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


priorityString : Priority -> Html msg
priorityString priority =
    let
        builder n suffix =
            span [] [ text n, sup [] [ text suffix ] ]
    in
        case priority of
            First ->
                builder "1" "st"

            Second ->
                builder "2" "nd"

            Third ->
                builder "3" "rd"


voteButtons : Vote -> ProjectId -> Html Msg
voteButtons userVote projectId =
    let
        ordButton priority =
            let
                active =
                    case voteN priority userVote of
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
                    , onClick (VoteFor priority projectId)
                    ]
                    [ priorityString priority ]
    in
        div [ class "btn-group" ]
            [ ordButton First
            , ordButton Second
            , ordButton Third
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
