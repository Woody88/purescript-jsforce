module Main where

import Effect.Class
import Prelude
import Salesforce.Connection
import Salesforce.Query.Internal
import Salesforce.Query.Types
import Salesforce.Types
import Control.Monad.Error.Class (try)
import Data.Either (Either, either)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Effect (Effect)
import Effect.Aff (launchAff_, Aff)
import Effect.Console (log, logShow)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (genericDecode, genericEncode, defaultOptions)
import Node.Process (lookupEnv)
import Salesforce.SObject (SObjectError, InsertResult, SObjectId(..), sobjectName)
import Salesforce.SObject as SObject

newtype Account = Account { "Name" :: String, "RecordTypeId" :: String,  "C_ShinkiKizon__c" :: String}

derive instance genericAccount :: Generic Account _

instance decodeAccount :: Decode Account where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}

instance encodeAccount :: Encode Account where
  encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}

instance showAccount :: Show Account where 
    show = genericShow

main :: Effect Unit 
main = do
    user   <- lookupEnv "SFUSERNAME"
    pswd   <- lookupEnv "SFPASSWORD"
    secret <- lookupEnv "SFSECRET"
    
    void $ sequence $ user >>= \u ->
    pswd >>= \p -> 
      secret >>= \s -> do 
        pure $ launchAff_ do  
            let soapConfig = Soap { username: username u
                                    , password: password p  $ secretToken s
                                    , instanceUrl: Nothing 
                                    , envType: Sandbox
                                    , version: 42.0
                                    }
            conn <- login soapConfig
            liftEffect $ either (log <<< show ) (\c -> launchAff_ do 
                liftEffect $ logShow c
                liftEffect $ app c) conn
                -- eitherAccount <- runSalesforceT queryAccount c
                -- liftEffect $ log $ either show show eitherAccount) conn

app :: Connection -> Effect Unit
app conn = launchAff_ do
  s <- flip runSalesforceT conn do
    eitherAcc <- try newAccount 
    pure $ either show show eitherAcc
  liftEffect $ logShow s
   

queryAccount :: Salesforce QueryError (QueryResult (Array Account)) 
queryAccount = query (SOQL "Select Id, Name From Account Limit 10")

newAccount :: Salesforce SObjectError SObjectId
newAccount = SObject.insert (sobjectName "Account") $ Account { "Name": "Test Account 3", "RecordTypeId": "01210000000RMRAAA4", "C_ShinkiKizon__c": "新規" }


