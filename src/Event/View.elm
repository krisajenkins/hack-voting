module Event.View exposing (root)

import Dict exposing (Dict)
import Event.State exposing (..)
import Event.Types exposing (..)
import Exts.Html.Bootstrap exposing (..)
import Exts.Maybe exposing (maybe)
import Exts.RemoteData exposing (..)
import Firebase.Auth exposing (User)
import Firebase.Common as Firebase
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


root : User -> Model -> Html Msg
root user model =
    div []
        [ maybe empty (errorView "There was a problem loading this event.") model.eventError
        , maybe empty (errorView "There was a problem saving your vote.") model.voteError
        , maybe empty (errorView "There was a problem saving your project.") model.projectError
        , case model.event of
            Success event ->
                eventView user event

            Failure err ->
                div [ class "alert alert-danger" ] [ text err ]

            Loading ->
                h2 [] [ i [] [ text "Waiting for event data..." ] ]

            NotAsked ->
                h2 [] [ text "Initialising." ]
        ]


errorView : String -> Firebase.Error -> Html msg
errorView title error =
    div [ class "alert alert-danger" ]
        [ h3 [] [ text title ]
        , text (toString error.code)
        , text error.message
        ]


eventView : User -> Event -> Html Msg
eventView user event =
    let
        userVote =
            Dict.get user.uid event.votes
                |> Maybe.withDefault initialVote
    in
        div []
            [ yourVote userVote
            , row
                [ div [ class "col-xs-12 col-sm-6" ]
                    [ projectsView userVote event.projects ]
                , div [ class "col-xs-12 col-sm-6" ]
                    [ votesView event ]
                ]
            ]


yourVote : Vote -> Html msg
yourVote userVote =
    h3 []
        [ case (List.map (voteN userVote) priorities) of
            (Just _) :: (Just _) :: (Just _) :: [] ->
                text "Thanks for voting!"

            _ ->
                text "Please use your remaining votes."
        ]


projectsView : Vote -> Dict String Project -> Html Msg
projectsView userVote projects =
    div []
        [ h2 [] [ text "Projects" ]
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
                    case voteN userVote priority of
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
                    , onClick
                        (VoteFor priority
                            (if active then
                                Nothing
                             else
                                Just projectId
                            )
                        )
                    ]
                    [ priorityString priority ]
    in
        div [ class "btn-group" ]
            (List.map ordButton priorities)


tally : Dict String Vote -> Dict ProjectId Int
tally =
    let
        increment =
            Just << (+) 1 << Maybe.withDefault 0
    in
        Dict.foldl
            (\_ vote acc ->
                List.foldl
                    (\accessor ->
                        voteN vote accessor
                            |> Maybe.map (flip Dict.update increment)
                            |> Maybe.withDefault identity
                    )
                    acc
                    priorities
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

        tallied =
            tally event.votes
                |> Dict.toList
    in
        div []
            [ h2 [] [ text "Votes" ]
            , if List.isEmpty tallied then
                empty
              else
                well
                    (tallied
                        |> List.map (voteBar event.projects maxCount)
                        |> List.intersperse (hr [] [])
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
            (toFloat voteCount / toFloat maxCount)
                * 100.0

        pct n =
            toString n ++ "%"
    in
        div []
            [ h3 []
                [ text name
                , text " "
                , badge voteCount
                ]
            , div
                [ style
                    [ ( "width", pct width )
                    , ( "margin", "15px 0" )
                    , ( "padding", "10px" )
                    , ( "background-color", "#3DF236" )
                    , ( "border", "solid 2px #28A024" )
                    , ( "border-radius", "10px" )
                    , ( "transition", "width 200ms" )
                    ]
                ]
                []
            ]


badge : Int -> Html msg
badge n =
    span [ class "badge" ] [ text (toString n) ]
