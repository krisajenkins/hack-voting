module State exposing (..)

import Exts.RemoteData exposing (..)
import Firebase.Auth as Firebase
import Firebase.Vote as Firebase
import Types exposing (..)


initialState : ( Model, Cmd Msg )
initialState =
    ( { auth = Loading
      , counter = 0
      , listening = True
      , votes = []
      , voteError = Nothing
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Sub.map AuthResponse Firebase.authResponse
        , Firebase.votes HeardVotes
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

        Increment ->
            let
                newModel =
                    { model | counter = model.counter + 1 }
            in
                ( newModel
                , case newModel.auth of
                    Success user ->
                        Firebase.voteSend
                            ( user.uid
                            , { counter = newModel.counter }
                            )

                    _ ->
                        Cmd.none
                )

        VoteError err ->
            ( { model | voteError = Just err }
            , Cmd.none
            )

        HeardVotes votes ->
            ( { model | votes = votes }
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


listenCmd : Model -> Cmd msg
listenCmd model =
    if model.listening then
        case model.auth of
            Success user ->
                Firebase.votesListen ()

            _ ->
                Firebase.votesSilence ()
    else
        Firebase.votesSilence ()
