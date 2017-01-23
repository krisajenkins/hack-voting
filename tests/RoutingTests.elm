module RoutingTests exposing (tests)

import Expect exposing (..)
import Navigation exposing (Location)
import Routing exposing (..)
import Test exposing (..)
import Types exposing (..)


tests : Test
tests =
    describe "Routing"
        [ fromUriTests
        , fromUriToUriTests
        ]


mockLocation : Location
mockLocation =
    { hash = ""
    , host = ""
    , hostname = ""
    , href = ""
    , origin = ""
    , password = ""
    , pathname = ""
    , port_ = ""
    , protocol = ""
    , search = ""
    , username = ""
    }


fromUriTests : Test
fromUriTests =
    let
        testPair ( location, view ) =
            test (location.hash ++ " -> " ++ toString view)
                (always
                    (equal view
                        (pathParser location)
                    )
                )
    in
        describe "fromUri"
            (List.map testPair
                (List.concat
                    [ [ ( { mockLocation | hash = "#/total/rubbish" }, NotFound )
                      ]
                    , exampleRoutes
                    ]
                )
            )


fromUriToUriTests : Test
fromUriToUriTests =
    let
        testPair ( location, view ) =
            test (location.hash ++ " -> " ++ toString view)
                (always
                    (equal location.hash
                        (pathRouter (pathParser location))
                    )
                )
    in
        describe "fromUri<->toUri"
            (List.map testPair exampleRoutes)


exampleRoutes : List ( Location, View )
exampleRoutes =
    [ ( { mockLocation | hash = "#/" }, FrontPage )
    , ( { mockLocation | hash = "#/event/foo" }, EventView "foo" )
    , ( { mockLocation | hash = "#/event/bar" }, EventView "bar" )
    ]
