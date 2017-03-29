module Routing exposing (Router, pathRouter, pathParser)

import Navigation exposing (Location)
import String exposing (split, join)
import Types exposing (View(..))
import UrlParser exposing (..)


type alias Router view =
    view -> String


pathRouter : Router View
pathRouter =
    toPath >> join "/" >> (++) "#/"


pathParser : Location -> View
pathParser =
    parseHash parser
        >> Maybe.withDefault NotFound


parser : Parser (View -> a) a
parser =
    oneOf
        [ map FrontPage <| top
        , map EventView <| s "event" </> string
        ]


toPath : View -> List String
toPath view =
    case view of
        FrontPage ->
            [ "" ]

        EventView id ->
            [ "event", id ]

        NotFound ->
            [ "404" ]
