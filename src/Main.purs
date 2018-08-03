module Main where

import Data.Tuple
import Effect.Aff
import Effect.Class
import Prelude
import Salesforce.Client
import Salesforce.SOQL.Query
import Salesforce.Types
import Control.Monad.Reader.Trans (ReaderT(..), runReaderT, ask)
import Data.Either (either, Either)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Traversable (traverse, traverse_, sequence)
import Effect (Effect)
import Effect.Aff.Class (liftAff)
import Effect.Console (log, logShow)
import Foreign.Class (class Decode, class Encode, decode, encode)
import Foreign.Generic (genericDecode, genericEncode, decodeJSON, genericEncodeJSON, encodeJSON, defaultOptions)
import Salesforce.Connection as Conn

newtype Account = Account 
  { "Name" :: String 
  , "Id"   :: String 
  }

derive instance genericAccount :: Generic Account _
-- derive instance genericSOQLResult' :: Generic SOQLResult' _

instance decodeAccount :: Decode Account where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}

instance encodeAccount :: Encode Account where
  encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}

instance showAccount :: Show Account where 
  show = genericShow 


main :: Effect Unit
main = mainNode

mainNode :: Effect Unit 
mainNode = do 
  c <- Conn.mkConnection loginOpts2
  launchAff_ do
    eitherLogin <- Conn.login c (Username "") (Password "" "")
    liftEffect $ either handleLoginError handleLoginSuccess eitherLogin
    liftEffect $ log "end app"
  

  where 
    loginOpts2 = { loginUrl: "https://test.salesforce.com" }

    handleLoginError (Conn.LoginError err) = log err

    handleLoginSuccess (Tuple conn userInfo) = do 
      log $ userInfo.id
      launchAff_ $ do
        liftEffect $ log "hello"
        eitherAccs <- queryAccounts conn
        liftEffect $ either handler (log <<< encodeJSON) eitherAccs
        liftEffect $ log $ "aff2"

    handler (QueryError x) = log x
    handler (SOQLParseError x) = logShow x

mainBrowser :: Effect Unit
mainBrowser = do
  client <- mkClient loginOpts
  launchAff_ do
    eitherLogin <- login {} client 
    liftEffect $ either handleLoginError handleLoginSuccess eitherLogin

  where 
    loginOpts = 
      {
        loginUrl: "https://test.salesforce.com",
        clientId: "3MVG9e2mBbZnmM6noi4JAoj2cZZ3nnpAkbtx8yuNkcKMOPKoGCYgDGlT2NqR_nrOlTNsNByN8fFz1sfudYzyq",
        redirectUri: "http://localhost:8080/",
        proxyUrl: "http://localhost:3123/proxy/"
      }

    handleLoginError result = case result of
      Cancelled -> log "cancelled"
      Error msg -> log msg

    handleLoginSuccess conn = do 
      log "connected"
      launchAff_ $ do
        v <- queryAccounts conn 
        liftEffect $ logShow v



queryAccounts :: Connection -> Aff (Either QueryError (Array Account))
queryAccounts conn = runSalesforceT q conn 
  where 
    q :: Salesforce QueryError (Array Account)
    q = query (SOQL "Select id, name From Account LIMIT 10") 

-- queryAccount2 :: forall r. SalesforceT (Either QueryError (QueryResult r))
-- queryAccount2 = do
--    conn <- ask 
--    pure $ queryString conn "Select Id, Name From Account Limit 20"

 
-- queryAccounts :: SOQL (Array {id, name})
-- queryAccount = SOQL \conn -> 
 