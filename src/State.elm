module State exposing (..)

import Dict exposing (Dict)
import Event.State as Event
import Exts.Dict exposing (indexBy)
import Firebase.Auth as Firebase
import RemoteData as RemoteData exposing (..)
import Response exposing (..)
import Types exposing (..)


init : View -> Response Model Msg
init view =
    ( { auth = Loading
      , events = Dict.empty
      , view = view
      }
    , Cmd.none
    )


update : Msg -> Model -> Response Model Msg
update msg model =
    case msg of
        UrlUpdate view ->
            ( { model | view = view }
            , Cmd.none
            )

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
                        |> indexBy (Tuple.first >> .id)
            in
                ( { model
                    | auth = response
                    , events = Dict.map (\k v -> Tuple.first v) events
                  }
                , events
                    |> Dict.map
                        (\eventId eventModel ->
                            Cmd.map (EventMsg eventId)
                                (Tuple.second eventModel)
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
