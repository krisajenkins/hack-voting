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
                [ votesView event ]
            , div [ class "col-xs-12 col-sm-6" ]
                [ projectsView userVote event.projects ]
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


tally : Dict String Vote -> Dict ProjectId Int
tally =
    let
        increment =
            Just << (+) 1 << Maybe.withDefault 0
    in
        Dict.foldl
            (\_ vote acc ->
                case vote.first of
                    Nothing ->
                        acc

                    Just projectId ->
                        Dict.update projectId increment acc
            )
            Dict.empty


votesView : Event -> Html msg
votesView event =
    let
        voteCounts =
            tally event.votes

        maxCount =
            voteCounts
                |> Dict.values
                |> List.maximum
                |> Maybe.withDefault 0
    in
        div []
            [ h1 [] [ text "Votes" ]
            , div []
                (tally event.votes
                    |> Dict.toList
                    |> List.map (voteBar event.projects maxCount)
                )
            ]


voteBar : Dict ProjectId Project -> Int -> ( ProjectId, Int ) -> Html msg
voteBar projects maxCount ( projectId, voteCount ) =
    let
        name =
            case Dict.get projectId projects of
                Nothing ->
                    projectId

                Just project ->
                    project.name

        width =
            (toFloat voteCount / (toFloat maxCount + 5))
                * 100.0

        pct n =
            toString n ++ "%"
    in
        div
            [ style
                [ ( "width", pct width )
                , ( "margin", "15px 0" )
                , ( "padding", "10px" )
                , ( "background-color", "aliceblue" )
                , ( "transition", "width 200ms" )
                ]
            ]
            [ badge voteCount
            , text " "
            , text name
            ]


badge : Int -> Html msg
badge n =
    span [ class "badge" ] [ text (toString n) ]
