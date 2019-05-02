module Test.Main where

import Prelude

import Effect (Effect)
import Test.Endpoints.QuerySpec as Query
import Test.RequestSpec as Request
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (run)

main :: Effect Unit
main = run [consoleReporter] do 
  Query.spec