module Salesforce.Internal where 

import Prelude

import Affjax (Response, ResponseFormatError, printResponseFormatError)
import Affjax as AX
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as ResponseFormat
import Affjax.StatusCode (StatusCode(..))
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader (class MonadReader, ask)
import Control.Plus (empty)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson, (.:), (.:?))
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.HTTP.Method (Method(..))
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Effect.Aff.Class (class MonadAff, liftAff)
import Salesforce.Connection.Types (Connection)
import Salesforce.Connection.Util (authorizationHeader, baseUrl, orgDomain)
import Salesforce.Query.Types (QueryEndpoint(..), SOQL(..))
import Salesforce.Types (Affjax, NTProxy(..), NetworkError, kind NetworkType)
import Salesforce.Util (Url)

affjaxNetwork :: NTProxy Affjax 
affjaxNetwork = NTProxy  

class Functor m <= HasNetworkType m (n :: NetworkType) | m -> n

class HasEndpoint sfapi where 
    endpointUrl :: Connection -> sfapi -> Url

instance hasQueryEndpoint :: HasEndpoint (QueryEndpoint r) where 
    endpointUrl conn queryEndpoint = do
        let formatSoqlToUrlParams = \soql -> String.replaceAll (Pattern " ") (Replacement "+") soql

        case queryEndpoint of 
            Query (SOQL soql)        -> baseUrl conn <> "/query/?q=" <> formatSoqlToUrlParams soql 
            QueryExplain (SOQL soql) -> baseUrl conn <> "/query/?explain=" <> formatSoqlToUrlParams soql 
            QueryNext nextRecordsUrl -> orgDomain conn <> nextRecordsUrl

-- | HasNetwork typclass which represent the request to salesforce
class HasNetwork m sfapi (n :: NetworkType) where 
    request :: 
        HasEndpoint sfapi  
        => HasNetworkType m n
        => MonadReader Connection m
        => MonadAff m
        => Applicative m
        => sfapi  
        -> m (Either NetworkError Json)


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

networkParseError :: String -> NetworkError
networkParseError message = {errorCode: "Network Parse Error", fields: empty, message} 

validateRequest :: Response (Either ResponseFormatError Json) -> Either NetworkError Json 
validateRequest {status: (StatusCode status), body} 
    | status >= 200 && status < 304 = either (throwError <<< networkParseError) (pure <<< identity) $ lmap printResponseFormatError body 
    | otherwise                     = throwError $ either (networkParseError <<< printResponseFormatError) decodeNetworkError body

decodeNetworkError :: Json -> NetworkError
decodeNetworkError json = do 
    let eitherNetworkErr = do
            obj       <- decodeJson json
            errorCode <- obj .: "errorCode"
            message   <- obj .: "message"
            fields    <- obj .:? "fields"
            (pure {errorCode, message, fields} :: Either String NetworkError)
    either networkParseError identity eitherNetworkErr