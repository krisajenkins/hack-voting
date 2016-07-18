module Routing exposing (hashParser, hashRouter, parser, toPath, toUri)

import Exts.String exposing (removePrefix)
import String exposing (split, join)
import Types exposing (View(..))
import UrlParser exposing (..)


type alias Router view =
    view -> String


(<<=) : a -> Parser a b -> Parser (b -> c) c
(<<=) =
    UrlParser.format


hashRouter : Router View
hashRouter =
    toPath >> join "/" >> (++) "#/"


hashParser : String -> View
hashParser hash =
    hash
        |> removePrefix "#/"
        |> UrlParser.parse identity parser
        |> Result.withDefault NotFound


parser : Parser (View -> a) a
parser =
    oneOf
        [ ProjectVotes <<= s ""
        , ProjectVotes <<= s "#"
        ]


toPath : Types.View -> List String
toPath view =
    case view of
        ProjectVotes ->
            [ "" ]

        NotFound ->
            [ "404" ]


toUri : View -> String
toUri view =
    case (toPath view |> String.join "/") of
        "" ->
            ""

        str ->
            "#/" ++ str
