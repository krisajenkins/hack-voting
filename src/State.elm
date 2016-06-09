module State exposing (..)

import Exts.RemoteData exposing (..)
import Firebase.Ports as Firebase
import Types exposing (..)


initialState : ( Model, Cmd Msg )
initialState =
    ( { auth = Loading
      , counter = 0
      , votes = []
      , voteError = Nothing
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Sub.map AuthResponse Firebase.authResponse
        , Firebase.listenToVotes Heard
        , Firebase.voteError VoteError
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "update" msg of
        Authenticate ->
            ( { model | auth = Loading }
            , Firebase.authenticate ()
            )

        AuthResponse response ->
            ( { model | auth = response }
            , case response of
                Success user ->
                    Firebase.watch ()

                _ ->
                    Cmd.none
            )

        Increment ->
            let
                newModel =
                    { model | counter = model.counter + 1 }
            in
                ( newModel
                , case newModel.auth of
                    Success user ->
                        Firebase.vote
                            ( user
                            , { counter = newModel.counter }
                            )

                    _ ->
                        Cmd.none
                )

        VoteError err ->
            ( { model | voteError = Just err }
            , Cmd.none
            )

        Heard n ->
            ( { model | votes = n }
            , Cmd.none
            )
