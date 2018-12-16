module Test.Main where

import Prelude
import Effect (Effect)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Discovery (discover)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)


main :: Effect Unit
main = do
  test

test :: Effect Unit 
test = discover "Salesforce.*Spec" >>= run [consoleReporter]
 
