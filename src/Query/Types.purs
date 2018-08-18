module Query.Types where

import Prelude (($))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Foreign (MultipleErrors)
import Foreign.Class (class Decode)
import Foreign.Generic (genericDecode, defaultOptions)

newtype SOQL result = SOQL String 
newtype QueryResult r
    = QueryResult { done           :: Boolean 
                  , totalSize      :: Int
                  , records        :: r
                  , nextRecordsUrl :: Maybe String 
                  }

data QueryEndpoint r = NextQuery String String | Query (SOQL r) String | QueryExplain (SOQL r) String | QueryReport String String
data QueryError = QueryError String | QueryParseError String

derive instance genericQueryResult :: Generic (QueryResult r) _

instance decodeQueryResult :: Decode r => Decode (QueryResult r) where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}
