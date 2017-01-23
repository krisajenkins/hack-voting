module Event.View exposing (root)

import Dict exposing (Dict)
import Event.State exposing (..)
import Event.Types exposing (..)
import Exts.Html.Bootstrap exposing (..)
import Exts.Maybe exposing (maybe)
import Firebase.Auth exposing (User)
import Firebase.Common as Firebase
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Markdown
import RemoteData exposing (..)


root : User -> Model -> Html Msg
root user model =
    div []
        [ maybe empty (errorView "There was a problem loading this event.") model.eventError
        , maybe empty (errorView "There was a problem saving your vote.") model.voteError
        , maybe empty (errorView "There was a problem saving your option.") model.optionError
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
            [ row
                [ div [ class "col-xs-12 col-sm-6" ]
                    [ optionsView userVote event ]
                , div [ class "col-xs-12 col-sm-6" ]
                    [ votesView event ]
                ]
            , row
                [ div [ class "col-xs-12 col-sm-6" ]
                    [ votingFeedback userVote ]
                ]
            ]


votingFeedback : Vote -> Html msg
votingFeedback userVote =
    div [ class "voting-feedback" ]
        [ uncurry div
            <| case (List.map (voteN userVote) priorities) of
                (Just _) :: (Just _) :: (Just _) :: [] ->
                    ( [ class "alert alert-info" ]
                    , [ h4 [] [ text "Thanks for voting!" ]
                      , p [] [ text "You can change your votes at any time." ]
                      ]
                    )

                _ ->
                    ( [ class "alert alert-warning" ]
                    , [ h4 [] [ text "Please use your remaining votes." ] ]
                    )
        ]


optionsView : Vote -> Event -> Html Msg
optionsView userVote event =
    div []
        [ h2 [] [ text event.title ]
        , div [ class "list-group" ]
            (event.options
                |> Dict.toList
                |> List.map (optionView userVote)
            )
        ]


optionView : Vote -> ( String, Option ) -> Html Msg
optionView userVote ( id, option ) =
    div [ class "list-group-item" ]
        [ div [ class "pull-right" ]
            [ voteButtons userVote id ]
        , h3 [] [ text option.name ]
        , Markdown.toHtml [] (Maybe.withDefault "" option.description)
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


voteButtons : Vote -> OptionId -> Html Msg
voteButtons userVote optionId =
    let
        ordButton priority =
            let
                active =
                    case voteN userVote priority of
                        Nothing ->
                            False

                        Just votedOptionId ->
                            votedOptionId == optionId
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
                                Just optionId
                            )
                        )
                    ]
                    [ priorityString priority ]
    in
        div [ class "btn-group" ]
            (List.map ordButton priorities)


tally : Dict String Vote -> Dict OptionId Int
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
            [ h2 [] [ text "Live Results" ]
            , if List.isEmpty tallied then
                empty
              else
                well
                    (tallied
                        |> List.map (voteBar event.options maxCount)
                        |> List.intersperse (hr [] [])
                    )
            ]


voteBar : Dict OptionId Option -> Int -> ( OptionId, Int ) -> Html msg
voteBar options maxCount ( optionId, voteCount ) =
    let
        name =
            case Dict.get optionId options of
                Nothing ->
                    optionId

                Just option ->
                    option.name

        width =
            (toFloat voteCount / toFloat maxCount)
                * 100.0

        pct n =
            toString n ++ "%"
    in
        div [ class "vote" ]
            [ h3 []
                [ text name
                , text " "
                , badge voteCount
                ]
            , div
                [ class "bar"
                , style
                    [ ( "width", pct width )
                    ]
                ]
                []
            ]


badge : Int -> Html msg
badge n =
    span [ class "badge" ] [ text (toString n) ]
