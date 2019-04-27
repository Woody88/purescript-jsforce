module Salesforce.Query.Types where 

import Prelude 
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Salesforce.Connection.Util (baseUrl)
import Salesforce.Internal (class HasEndpoint)


newtype SOQL r = SOQL String  
data QueryEndpoint r = Query (SOQL r) | QueryExplain (SOQL r)

instance hasQueryEndpoint :: HasEndpoint (QueryEndpoint r) where 
    endpointUrl conn queryEndpoint = do
        let formatSoqlToUrlParams = \soql -> String.replaceAll (Pattern " ") (Replacement "+") soql

        case queryEndpoint of 
            Query (SOQL soql)        -> baseUrl conn <> "/query/?q=" <> formatSoqlToUrlParams soql 
            QueryExplain (SOQL soql) -> baseUrl conn <> "/query/?explain=" <> formatSoqlToUrlParams soql 