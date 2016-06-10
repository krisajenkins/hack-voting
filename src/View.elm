module View exposing (root)

import Exts.RemoteData exposing (..)
import Firebase.Vote as Firebase
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
        , button
            [ class "btn btn-primary"
            , onClick ToggleListen
            ]
            [ text
                (if model.listening then
                    "Silence"
                 else
                    "Listen"
                )
            ]
        , button
            [ class "btn btn-success"
            , onClick Increment
            ]
            [ text "Increment" ]
        , div [ class "list-group" ]
            (List.map showVote model.votes)
        , div [] [ code [] [ text (toString model) ] ]
        ]


showVote : ( String, Firebase.Vote ) -> Html msg
showVote ( path, vote ) =
    div [ class "list-group-item" ]
        [ h3 [] [ text path ]
        , div [] [ code [] [ text (toString vote) ] ]
        ]
