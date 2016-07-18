module Event.State exposing (..)

import Dict
import Event.Ports exposing (..)
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


initialState : ( Model, Cmd Msg )
initialState =
    ( { event = Loading
      , eventError = Nothing
      , voteError = Nothing
      , optionError = Nothing
      }
    , eventListen ()
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ event (Decode.decodeString decodeEvent >> HeardEvent)
        , eventError EventError
        , voteSendError VoteError
        , optionSendError OptionError
        ]


update : User -> Msg -> Model -> ( Model, Cmd Msg )
update user msg model =
    case msg of
        HeardEvent response ->
            ( { model | event = RemoteData.fromResult response }
            , Cmd.none
            )

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
                        , voteSend ( user.uid, newVote )
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
