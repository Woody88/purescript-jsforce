module Salesforce.Query.Types where 

import Salesforce.Types (NetworkError)
newtype SOQL r = SOQL String  

data QueryEndpoint r = Query (SOQL r) | QueryExplain (SOQL r)

type QueryError r = ( queryError      :: NetworkError
                    , queryParseError :: String 
                    | r
                    )

