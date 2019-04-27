module Salesforce.Connection.Util where 

import Prelude

import Salesforce.Connection.Types (Connection(..), OrgEnvironment(..))
import Salesforce.Util (Url)

orgDomain :: Connection -> Url
orgDomain (Connection { orgEnvironment })
    | orgEnvironment == Production = "https://login.salesforce.com"
    | otherwise                    = "https://test.salesforce.com" 

versionTag :: Connection -> String 
versionTag (Connection {version}) = "v" <> show version 

baseUrl :: Connection -> Url 
baseUrl conn =  (orgDomain conn) <> "/service/data/" <> (versionTag conn) 