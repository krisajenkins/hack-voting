module StateTest exposing (tests)

import ElmTest exposing (..)


-- import Check exposing (..)
-- import Check.Producer exposing (..)
-- import Check.Test exposing (evidenceToTest)
-- import State exposing (..)
-- import Types exposing (..)


tests : Test
tests =
    ElmTest.suite "State" []



--         [ updateTests
--         , evidenceToTest (quickCheck updateClaims)
--         ]
-- updateTests : Test
-- updateTests =
--     ElmTest.suite "update"
--         [ defaultTest
--             (assertEqual { counter = 5 }
--                 (fst (update Increment { counter = 4 }))
--             )
--         ]
-- updateClaims : Claim
-- updateClaims =
--     Check.suite "update"
--         [ claim "Increment adds one."
--             `that` (\n -> { counter = n + 1 })
--             `is` (\n -> (fst (update Increment { counter = n })))
--             `for` int
--         ]
