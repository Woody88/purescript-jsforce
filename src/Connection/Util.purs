module Salesforce.Connection.Util where 

import Prelude

import Data.Maybe (maybe)
import Salesforce.Connection.Types (Connection(..), OrgEnvironment(..), getToken)
import Salesforce.Util (Url)

orgDomain :: Connection -> Url
orgDomain (Connection { orgEnvironment })
    | orgEnvironment == Production = "https://login.salesforce.com"
    | otherwise                    = "https://test.salesforce.com" 

versionTag :: Connection -> String 
versionTag (Connection {version}) = "v" <> show version 

baseUrl :: Connection -> Url 
baseUrl conn =  (orgDomain conn) <> "/service/data/" <> (versionTag conn) 

authorizationHeader :: Connection -> String 
authorizationHeader (Connection {access_token, token_type}) = maybe mempty (mkAuthHeader (getToken access_token)) token_type
    where 
        mkAuthHeader token tkn_type= tkn_type <> " " <> token