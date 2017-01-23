module Tests exposing (tests)

{-| The main entry point for the tests.

@docs main
-}

import RoutingTests
import Test exposing (..)


tests : Test
tests =
    describe "All"
        [ RoutingTests.tests
        ]
