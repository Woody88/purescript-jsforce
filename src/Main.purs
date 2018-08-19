module Main where

import Salesforce.Connection
import Salesforce.Types
import Prelude
import Query.Internal
import Query.Types
import Data.Traversable (sequence)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class
import Effect.Aff (launchAff_)
import Effect.Console (log, logShow)
import Foreign.Class (class Decode, decode)
import Foreign.Generic (genericDecode, defaultOptions)
import Node.Process (lookupEnv)

newtype Account = Account { "Id" :: String, "Name" :: String }

derive instance genericAccount :: Generic Account _

instance decodeQueryResult :: Decode Account where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}

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
                liftEffect $ logShow conn
                eitherAccount <- runSalesforceT queryAccount c
                liftEffect $ log $ either show show eitherAccount) conn


queryAccount :: Salesforce QueryError (QueryResult (Array Account)) 
queryAccount = query (SOQL "Select Id, Name From Account Limit 10")




