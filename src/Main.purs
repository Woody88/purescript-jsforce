module Main where

import Prelude
import Control.Monad.Reader.Trans (ReaderT(..), runReaderT, ask)
import Data.Tuple
import Data.Either (either, Either)
import Effect (Effect)
import Effect.Class
import Effect.Console (log, logShow)
import Effect.Aff
import Effect.Aff.Class (liftAff)
import Salesforce.Client
import Salesforce.Connection as Conn
import Salesforce.Types
import Salesforce.SOQL.Query

main :: Effect Unit
main = do
  c <- Conn.mkConnection loginOpts2
  launchAff_ do
    eitherLogin <- Conn.login c (Username "..") (Password "..." "..")
    liftEffect $ either handleLoginError2 handleLoginSuccess2 eitherLogin

  where 
    loginOpts2 = { loginUrl: "https://test.salesforce.com" }
    
    loginOpts = 
      {
        loginUrl: "https://test.salesforce.com",
        clientId: "...",
        redirectUri: "http://localhost:8080/",
        proxyUrl: "http://localhost:3123/proxy/"
      }
    handleLoginError2 (Conn.LoginError err) = log err

    handleLoginError result = case result of
      Cancelled -> log "cancelled"
      Error msg -> log msg

    handleLoginSuccess conn = do 
      log "connected"
      launchAff_ $ runSalesforce queryAccounts conn

    handleLoginSuccess2 (Tuple conn userInfo) = do 
      log $ userInfo.id


    -- handleQuery = either handleLoginQueryError handleLoginQuerySuccess


queryAccounts :: SalesforceM Unit
queryAccounts = SalesforceM \conn -> do
    liftEffect $ getAccounts conn


-- queryAccount2 :: forall r. SalesforceT (Either QueryError (QueryResult r))
-- queryAccount2 = do
--    conn <- ask 
--    pure $ queryString conn "Select Id, Name From Account Limit 20"

 
handleLoginQueryError :: QueryError -> Aff Unit
handleLoginQueryError (QueryError result) = liftEffect $ log result

handleLoginQuerySuccess :: forall r. QueryResult r -> Aff Unit
handleLoginQuerySuccess result = liftEffect $ log "done"

-- queryAccounts :: SOQL (Array {id, name})
-- queryAccount = SOQL \conn -> 
 