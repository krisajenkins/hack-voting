module State exposing (..)

import Dict
import Exts.RemoteData as RemoteData exposing (..)
import Firebase.Auth as Firebase
import Firebase.Event as Firebase
import Json.Decode as Decode
import Rest
import Types exposing (..)


initialState : ( Model, Cmd Msg )
initialState =
    ( { auth = Loading
      , listening = True
      , event = Loading
      , eventError = Nothing
      , voteError = Nothing
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Sub.map AuthResponse Firebase.authResponse
        , Firebase.event (Decode.decodeString Rest.decodeEvent >> HeardEvent)
        , Firebase.eventError EventError
        , Firebase.voteSendError VoteError
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Authenticate ->
            ( { model | auth = Loading }
            , Firebase.authenticate ()
            )

        AuthResponse response ->
            let
                newModel =
                    { model | auth = response }
            in
                ( newModel, listenCmd newModel )

        VoteError err ->
            ( { model | voteError = Just err }
            , Cmd.none
            )

        HeardEvent response ->
            ( { model | event = RemoteData.fromResult response }
            , Cmd.none
            )

        EventError error ->
            ( { model | eventError = Just error }
            , Cmd.none
            )

        ToggleListen ->
            let
                newModel =
                    { model | listening = not model.listening }
            in
                ( newModel
                , listenCmd newModel
                )

        VoteFor projectId n ->
            case model.auth of
                Success user ->
                    case model.event of
                        Success event ->
                            let
                                oldVote =
                                    Dict.get user.uid event.votes
                                        |> Maybe.withDefault Firebase.initialVote

                                newVote =
                                    case n of
                                        1 ->
                                            { oldVote | first = Just projectId }

                                        2 ->
                                            { oldVote | second = Just projectId }

                                        3 ->
                                            { oldVote | third = Just projectId }

                                        _ ->
                                            oldVote
                            in
                                ( model
                                , Firebase.voteSend ( user.uid, newVote )
                                )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )


listenCmd : Model -> Cmd msg
listenCmd model =
    if model.listening then
        case model.auth of
            Success user ->
                Firebase.eventListen ()

            _ ->
                Firebase.eventSilence ()
    else
        Firebase.eventSilence ()
