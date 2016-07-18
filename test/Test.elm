module Test exposing (main)

{-| The main entry point for the tests.

@docs main
-}

import ElmTest exposing (..)
import RoutingTests
import StateTest


tests : Test
tests =
    suite "All"
        [ StateTest.tests
        , RoutingTests.tests
        ]


{-| Run the test suite under node.
-}
main : Program Never
main =
    runSuite tests
