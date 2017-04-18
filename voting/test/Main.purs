module Test.Main where

import Prelude
import TypesTests as TypesTests
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Random (RANDOM)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)

main :: forall eff. Eff (testOutput :: TESTOUTPUT, avar :: AVAR, console :: CONSOLE, random :: RANDOM | eff) Unit
main = runTest do
  TypesTests.all
