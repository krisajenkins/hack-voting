module State where

import Data.Map as Map
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.State (modify)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Map (Map)
import Event.State (init) as Event
import Event.Types (EventId(..))
import Event.Types (State) as Event
import Halogen (ComponentDSL, raise)
import Network.RemoteData (RemoteData(..))
import Prelude (type (~>), bind, pure, ($))
import Types (Message(..), Query(..), SomeUser, State, View(FrontPage))

init :: State
init =
    { auth: NotAsked
    , events: Map.empty
    , view: FrontPage
    }

eval :: forall eff. Query ~> ComponentDSL State Query Message (Aff (console :: CONSOLE | eff))
eval (UpdateView view next) = do
  modify (_ { view = view })
  pure next
eval (AuthResponse response next) = do
  modify $ \state -> state
             { auth = response
             , events = authEvents response state.events
             }
  raise $ WatchEvent $ EventId "projects"
  pure next
eval (Authenticate next) = pure next
eval (EventMsg _ _ next) = pure next
eval (SomeEvent (Left err) next) = do
  liftEff $ logShow err
  pure next
eval (SomeEvent (Right snapshot) next) = do
  liftEff $ log "Got a snapshot"
  pure next

authEvents ::
  RemoteData Error SomeUser
  -> Map EventId Event.State
  -> Map EventId Event.State
authEvents (Success user) events = do
  foldl create events
    [ EventId "languages"
    , EventId "projects"
    ]
  where
    create :: Map EventId Event.State -> EventId -> Map EventId Event.State
    create map eventId = Map.insert eventId (Event.init eventId) map
authEvents _ m = m


-- TODO When successfully authenticated, subscribe to the "languages"
-- and "projects" events.

    --     Authenticate ->
    --         ( { model | auth = Loading }
    --         , Firebase.authenticate ()
    --         )

    --     AuthResponse ((Success _) as response) ->
    --         let
    --             events =
    --                 [ "languages"
    --                 , "projects"
    --                 ]
    --                     |> List.map Event.initialState
    --                     |> indexBy (Tuple.first >> .id)
    --         in
    --             ( { model
    --                 | auth = response
    --                 , events = Dict.map (\k v -> Tuple.first v) events
    --               }
    --             , events
    --                 |> Dict.map
    --                     (\eventId eventModel ->
    --                         Cmd.map (EventMsg eventId)
    --                             (Tuple.second eventModel)
    --                     )
    --                 |> Dict.values
    --                 |> Cmd.batch
    --             )

    --     AuthResponse response ->
    --         ( { model | auth = response }
    --         , Cmd.none
    --         )

    --     EventMsg eventId submsg ->
    --         case ( model.auth, Dict.get eventId model.events ) of
    --             ( Success user, Just eventModel ) ->
    --                 Event.update user submsg eventModel
    --                     |> mapModel
    --                         (\eventModel ->
    --                             { model | events = Dict.insert eventId eventModel model.events }
    --                         )
    --                     |> mapCmd (EventMsg eventId)

    --             _ ->
    --                 ( model, Cmd.none )
