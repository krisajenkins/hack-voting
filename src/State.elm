module State exposing (..)

import Dict exposing (Dict)
import Event.State as Event
import Exts.Dict exposing (indexBy)
import Firebase.Auth as Firebase
import RemoteData as RemoteData exposing (..)
import Response exposing (..)
import Types exposing (..)


initialState : View -> Response Model Msg
initialState initialView =
    ( { auth = Loading
      , events = Dict.empty
      , view = initialView
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map AuthResponse Firebase.authResponse
        , model.events
            |> Dict.map
                (\eventId eventModel ->
                    Sub.map (EventMsg eventId)
                        (Event.subscriptions eventModel)
                )
            |> Dict.values
            |> Sub.batch
        ]


update : Msg -> Model -> Response Model Msg
update msg model =
    case Debug.log "MSG" msg of
        Authenticate ->
            ( { model | auth = Loading }
            , Firebase.authenticate ()
            )

        AuthResponse ((Success _) as response) ->
            let
                events =
                    [ "languages"
                    , "projects"
                    ]
                        |> List.map Event.initialState
                        |> indexBy (fst >> .id)
            in
                ( { model
                    | auth = response
                    , events = Dict.map (\k v -> fst v) events
                  }
                , events
                    |> Dict.map
                        (\eventId eventModel ->
                            Cmd.map (EventMsg eventId)
                                (snd eventModel)
                        )
                    |> Dict.values
                    |> Cmd.batch
                )

        AuthResponse response ->
            ( { model | auth = response }
            , Cmd.none
            )

        EventMsg eventId submsg ->
            case ( model.auth, Dict.get eventId model.events ) of
                ( Success user, Just eventModel ) ->
                    Event.update user submsg eventModel
                        |> mapModel
                            (\eventModel ->
                                { model | events = Dict.insert eventId eventModel model.events }
                            )
                        |> mapCmd (EventMsg eventId)

                _ ->
                    ( model, Cmd.none )


urlUpdate : View -> Model -> Response Model Msg
urlUpdate view model =
    ( { model | view = view }, Cmd.none )
