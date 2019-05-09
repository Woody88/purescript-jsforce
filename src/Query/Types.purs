module Salesforce.Query.Types where 

import Prelude

import Affjax as AX
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.Reader (ask)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, getField)
import Data.Argonaut.Decode.Class (decodeJArray)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.HTTP.Method (Method(..))
import Data.Newtype (class Newtype, unwrap)
import Data.String (Replacement(..), Pattern(..))
import Data.String as String
import Data.Traversable (traverse)
import Effect.Aff.Class (liftAff)
import Salesforce.Connection.Util (authorizationHeader, baseUrl, orgDomain)
import Salesforce.Internal (class HasEndpoint, class HasNetwork, endpointUrl, validateRequest)
import Salesforce.Types (Affjax, NetworkError)

newtype SOQL r = SOQL String  

data QueryEndpoint r = Query (SOQL r) | QueryExplain (SOQL r) | QueryNext String 

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

instance hasQueryEndpoint :: HasEndpoint (QueryEndpoint r) where 
    endpointUrl conn queryEndpoint = do
        let formatSoqlToUrlParams = \soql -> String.replaceAll (Pattern " ") (Replacement "+") soql

        case queryEndpoint of 
            Query (SOQL soql)        -> baseUrl conn <> "/query/?q=" <> formatSoqlToUrlParams soql 
            QueryExplain (SOQL soql) -> baseUrl conn <> "/query/?explain=" <> formatSoqlToUrlParams soql 
            QueryNext nextRecordsUrl -> orgDomain conn <> nextRecordsUrl

instance hasQueryNetwork :: HasNetwork m (QueryEndpoint r) Affjax where 
    request queryEndpoint = do
        conn <- ask
        let
            url = endpointUrl conn queryEndpoint
            authHeader = authorizationHeader conn
        res <- liftAff $ AX.request $ AX.defaultRequest
            { url = url
            , method = Left GET
            , responseFormat = ResponseFormat.json
            , headers = [ RequestHeader "Authorization" authHeader ]
            }
        pure $ validateRequest res

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