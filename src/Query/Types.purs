module Salesforce.Query.Types where 

import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Generic.Rep (class Generic)
import Salesforce.Types (NetworkError)
newtype SOQL r = SOQL String  

data QueryEndpoint r = Query (SOQL r) | QueryExplain (SOQL r)

type QueryError r = ( queryError      :: NetworkError
                    , queryParseError :: String 
                    | r
                    )
newtype QueryResult sobject 
    = QueryResult { done      :: Boolean 
                  , totalSize :: Int
                  , records   :: Array sobject     
                  }

derive instance genericQueryResult :: Generic (QueryResult sobject) _ 

instance decodeJsonQueryResult :: DecodeJson sobject => DecodeJson (QueryResult sobject) where 
    decodeJson = genericDecodeJson