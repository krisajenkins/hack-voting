module RoutingTests exposing (tests)

import ElmTest exposing (..)
import Routing exposing (..)
import Types exposing (..)


tests : Test
tests =
    suite "Routing"
        [ fromUriTests
        , fromUriToUriTests
        ]


fromUriTests : Test
fromUriTests =
    suite "fromUri"
        (List.map (\( uri, view ) -> defaultTest (assertEqual view (hashParser uri)))
            (( "#/total/rubbish", NotFound )
                :: ( "#", FrontPage )
                :: exampleRoutes
            )
        )


fromUriToUriTests : Test
fromUriToUriTests =
    suite "fromUri->toUri"
        (List.map (\( uri, view ) -> defaultTest (assertEqual uri (toUri (hashParser uri))))
            exampleRoutes
        )


exampleRoutes : List ( String, Types.View )
exampleRoutes =
    [ ( "", FrontPage )
    , ( "#/event/foo", EventView "foo" )
    , ( "#/event/bar", EventView "bar" )
    ]
