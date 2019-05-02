module Salesforce.Query.Types where 

import Prelude

import Data.Argonaut.Decode (class DecodeJson, decodeJson, getField)
import Data.Argonaut.Decode.Class (decodeJArray)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap)
import Data.Traversable (traverse)
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

derive instance newtypeQueryResult :: Newtype (QueryResult sobject) _ 
derive instance genericQueryResult :: Generic (QueryResult sobject) _ 

instance decodeJsonQueryResult :: DecodeJson sobject => DecodeJson (QueryResult sobject) where 
    decodeJson json = do
        obj <- decodeJson json 
        done <- getField obj "done"
        totalSize <- getField obj "totalSize"
        arr <- decodeJArray =<< getField obj "records"
        records <- traverse decodeJson arr 
        pure $ QueryResult { done, totalSize, records}

totalSize :: forall sobject. QueryResult sobject -> Int 
totalSize =  _.totalSize <<< unwrap

records :: forall sobject. QueryResult sobject -> Array sobject 
records = _.records <<< unwrap