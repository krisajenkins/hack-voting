module Event.State exposing (..)

import Dict
import Event.Ports as Ports
import Event.Rest exposing (..)
import Event.Types exposing (..)
import Firebase.Auth exposing (User)
import Json.Decode as Decode
import RemoteData as RemoteData exposing (..)


initialVote : Vote
initialVote =
    { first = Nothing
    , second = Nothing
    , third = Nothing
    }


initialState : EventId -> ( Model, Cmd Msg )
initialState eventId =
    ( { id = eventId
      , event = Loading
      , eventError = Nothing
      , voteError = Nothing
      , optionError = Nothing
      }
    , Ports.eventListen eventId
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Ports.event
            (\( eventId, payload ) ->
                if model.id == eventId then
                    Decode.decodeString decodeEvent payload |> HeardEvent
                else
                    Ignore
            )
        , Ports.eventError EventError
        , Ports.voteSendError VoteError
        , Ports.optionSendError OptionError
        ]


update : User -> Msg -> Model -> ( Model, Cmd Msg )
update user msg model =
    case msg of
        HeardEvent response ->
            ( { model | event = RemoteData.fromResult response }
            , Cmd.none
            )

        Ignore ->
            ( model, Cmd.none )

        EventError error ->
            ( { model | eventError = Just error }
            , Cmd.none
            )

        VoteFor priority optionId ->
            case model.event of
                Success event ->
                    let
                        oldVote =
                            Dict.get user.uid event.votes
                                |> Maybe.withDefault initialVote

                        newVote =
                            case priority of
                                First ->
                                    { oldVote | first = optionId }

                                Second ->
                                    { oldVote | second = optionId }

                                Third ->
                                    { oldVote | third = optionId }
                    in
                        ( model
                        , Ports.voteSend ( model.id, user.uid, newVote )
                        )

                _ ->
                    ( model, Cmd.none )

        VoteError err ->
            ( { model | voteError = Just err }
            , Cmd.none
            )

        OptionError err ->
            ( { model | optionError = Just err }
            , Cmd.none
            )
