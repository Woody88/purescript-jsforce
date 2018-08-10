module Main where

import Data.Tuple
import Effect.Aff
import Effect.Class
import Prelude
import Salesforce.Client
import Salesforce.SOQL.Query
import Salesforce.Types

import Control.Monad.Reader.Trans (ReaderT(..), runReaderT, ask)
import Data.Either (either, Either(..), isLeft)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)
import Data.Traversable (traverse, traverse_, sequence)
import Effect (Effect)
import Effect.Aff.Class (liftAff)
import Effect.Console (log, logShow)
import Foreign.Class (class Decode, class Encode, decode, encode)
import Foreign.Generic (genericDecode, genericEncode, decodeJSON, genericEncodeJSON, encodeJSON, defaultOptions)
import Node.Process (lookupEnv)
import Salesforce.Apex as Apex
import Salesforce.ApprovalProcess as SFAP
import Salesforce.Connection (RequestError)
import Salesforce.Connection as Conn
import Salesforce.SObject.Types (UserInfo(..))
import Simple.JSON as JSON

newtype Account = Account 
  { "Name"             :: String 
  , "Id"               :: String 
  , "RecordTypeId"     :: String 
  , "C_ShinkiKizon__c" :: String
  }

derive instance newtypeAccount :: Newtype Account _ 
derive instance genericAccount :: Generic Account _

instance decodeAccount :: Decode Account where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}

instance encodeAccount :: Encode Account where
  encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}

instance jsomAccount :: JSON.ReadForeign Account where 
  readImpl = genericDecode $ defaultOptions {unwrapSingleConstructors = true}

instance writeJsonAccount :: JSON.WriteForeign Account where 
  writeImpl = genericEncode $ defaultOptions {unwrapSingleConstructors = true}

instance showAccount :: Show Account where 
  show = genericShow 


main :: Effect Unit
main = mainNode

mainNode :: Effect Unit 
mainNode = do 
  username <- lookupEnv "SFUSERNAME"
  password <- lookupEnv "SFPASSWORD"
  secret   <- lookupEnv "SFSECRET"
  void $ sequence $ username >>= \u ->
    password >>= \p -> 
      secret >>= \s -> do 
        pure $ launchAff_ $ app (Username u) (Password p s) 

app :: Username -> Password -> Aff Unit
app user pswd = do
  c <- liftEffect $ Conn.mkConnection loginOpts2
  eitherLogin <- Conn.login c user pswd
  liftEffect $ either logShow runSalesforceApp eitherLogin
  where 
    loginOpts2 = { loginUrl: "https://test.salesforce.com" }
 

runSalesforceApp :: (Tuple Connection UserInfo) -> Effect Unit 
runSalesforceApp (Tuple conn userInfo) = do 
  log $ userInfo.id
  launchAff_ $ flip runSalesforceT conn do
    liftEffect $ log "running Salesforce..."
    eitherSubmit <- try $ SFAP.submit (SFAP.ContextId "aB8N00000004Ca6") (SFAP.Comments mempty) {}
    liftEffect $ log $ either show JSON.writeJSON eitherSubmit

    eitherGetAccount  <- try $ getApexAccount "/v1/accounts/001N000001KLDcN" {}
    liftEffect $ log $ either show JSON.writeJSON eitherGetAccount

    eitherCreateAccount <- createAccount $ Account $ {"RecordTypeId":"01210000000RMRAAA4","Name":"出船鮨 Test2","Id":"","C_ShinkiKizon__c":"新規"}
    eitherAccount <- try $ queryAccounts --(getAccount $ sfdcId "00110000011280xAAA" <<< sobjectName $ "Account") 
    liftEffect $ either handler (log <<< encodeJSON) eitherAccount
    liftEffect $ either handler (\accs -> launchAff_ $ flip runSalesforceT conn do deleteAccounts accs) eitherAccount
    -- eitherAccount <- try $ (getAccount $ sfdcId "00110000011280xAAA" <<< sobjectName $ "Account") 
    -- liftEffect $ log $ either (const "failed retrieve") encodeJSON eitherAccount
    -- eitherCreateAccount <- try $ createAccount $ Account $ {"RecordTypeId":"01210000000RMRAAA4","Name":"出船鮨 Test2","Id":"","C_ShinkiKizon__c":"新規"}
    -- liftEffect $ either (const $ log "faile create") (handleCreateSuccess conn) eitherCreateAccount  
  where 
    handler (Conn.Error x) = log x
    handler (Conn.DecodeError x) = logShow x
    handler _ = log $ "unknown error"

handleCreateSuccess :: Connection -> Account -> Effect Unit
handleCreateSuccess conn acc@(Account rec) = do
  log $ encodeJSON acc
  launchAff_ $ flip runSalesforceT conn do
    eitherAccount <- try $ (getAccount $ (sfdcId $ rec."Id") <<< sobjectName $ "Account")
    liftEffect $ log $ either (const "failed retrieve") encodeJSON eitherAccount

-- mainBrowser :: Effect Unit
-- mainBrowser = do
--   client <- mkClient loginOpts
--   launchAff_ do
--     eitherLogin <- login {} client 
--     liftEffect $ either handleLoginError handleLoginSuccess eitherLogin

--   where 
--     loginOpts = 
--       {
--         loginUrl: "https://test.salesforce.com",
--         clientId: "3MVG9e2mBbZnmM6noi4JAoj2cZZ3nnpAkbtx8yuNkcKMOPKoGCYgDGlT2NqR_nrOlTNsNByN8fFz1sfudYzyq",
--         redirectUri: "http://localhost:8080/",
--         proxyUrl: "http://localhost:3123/proxy/"
--       }

--     handleLoginError result = case result of
--       Cancelled -> log "cancelled"
--       Error msg -> log msg

--     handleLoginSuccess conn = do 
--       log "connected"
--       launchAff_ $ do
--         v <- queryAccounts conn 
--         liftEffect $ logShow v

getApexAccount :: forall opts. String -> opts -> Salesforce Conn.RequestError Account 
getApexAccount = Apex.get  

getAccount :: SFId -> Salesforce Conn.RequestError Account 
getAccount = Conn.retrieve

createAccount :: Account -> Salesforce Conn.RequestError Account
createAccount = Conn.create (sobjectName "Account") 

updateAccount :: Account -> Salesforce Conn.RequestError RecordResult 
updateAccount (Account acc) = Conn.update (sobjectName "Account") (Account $ acc {"Name" = "出船鮨"})

queryAccounts :: Salesforce Conn.RequestError (Array Account)
queryAccounts = query (SOQL "Select id, name, RecordTypeId, C_ShinkiKizon__c From Account Where name = '出船鮨 Test2' ")

deleteAccounts :: Array Account -> Salesforce Conn.RequestError Unit
deleteAccounts accnts = do 
  liftEffect $ log "deleting accounts"
  arrResult <- traverse (\(Account a) -> Conn.destroy $ sfdcId a."Id" <<< sobjectName $ "Account") accnts
  void $ liftEffect $ traverse (\r -> log r.id) arrResult
  pure unit
  --void $ liftEffect $  log a

-- queryAccount2 :: forall r. SalesforceT (Either QueryError (QueryResult r))
-- queryAccount2 = do
--    conn <- ask 
--    pure $ queryString conn "Select Id, Name From Account Limit 20"
-- queryAccounts :: SOQL (Array {id, name})
-- queryAccount = SOQL \conn -> 
 