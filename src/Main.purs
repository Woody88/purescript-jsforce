module Main where

import Prelude
import Data.Either (either)
import Effect (Effect)
import Effect.Class
import Effect.Console (log)
import Effect.Aff
import Salesforce.Client

main :: Effect Unit
main = launchAff_ do
  client <- liftEffect $ mkClient {}
  eitherLogin <- login {} client 
  liftEffect $ either handleLoginError handleLoginSuccess eitherLogin

  where 
    handleLoginError result = case result of
      Cancelled -> log "cancelled"
      Error msg -> log msg
    handleLoginSuccess _ = log "connected"



