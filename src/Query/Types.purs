module Salesforce.Query.Types where 

import Prelude

import Affjax as AX
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.Except.Checked (ExceptV)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Data.Variant (SProxy(..), Variant, inj)
import Effect.Aff (Aff)
import Salesforce.Connection.Util (baseUrl, authorizationHeader)
import Salesforce.Internal (class HasEndpoint, class HasNetwork, NetworkError, request, endpointUrl, validateRequest)
import Salesforce.Types (SalesforceV, salesforce)
import Type.Row (type (+))

newtype SOQL r = SOQL String  

data QueryEndpoint r = Query (SOQL r) | QueryExplain (SOQL r)

type QueryError r = ( queryError      :: NetworkError
                    , queryParseError :: String 
                    | r
                    )

query :: forall sobject r. DecodeJson sobject => SOQL sobject -> SalesforceV (QueryError + r) sobject
query soql = salesforce \conn -> do 
    res <- (lmap queryError) <$> (request conn $ Query soql) 
    pure $ res >>= lmap queryParseError <<< decodeJson 

queryExplain :: forall sobject r. DecodeJson sobject => SOQL sobject -> SalesforceV (QueryError + r) sobject
queryExplain soql = salesforce \conn -> do 
    res <- (lmap queryError) <$> (request conn $ QueryExplain soql) 
    pure $ res >>= lmap queryParseError <<< decodeJson 

queryError :: forall r. NetworkError -> Variant (queryError :: NetworkError | r) 
queryError = inj (SProxy :: SProxy "queryError") 

queryParseError :: forall r. String -> Variant (queryParseError :: String | r) 
queryParseError = inj (SProxy :: SProxy "queryParseError") 

instance hasQueryEndpoint :: HasEndpoint (QueryEndpoint r) where 
    endpointUrl conn queryEndpoint = do
        let formatSoqlToUrlParams = \soql -> String.replaceAll (Pattern " ") (Replacement "+") soql

        case queryEndpoint of 
            Query (SOQL soql)        -> baseUrl conn <> "/query/?q=" <> formatSoqlToUrlParams soql 
            QueryExplain (SOQL soql) -> baseUrl conn <> "/query/?explain=" <> formatSoqlToUrlParams soql 

instance hasQueryNetwork :: HasNetwork Aff (QueryEndpoint r) where 
    request conn queryEndpoint = do 
        let url        = endpointUrl conn queryEndpoint 
            authHeader = authorizationHeader conn 
        res <- AX.request (AX.defaultRequest { url = url
                                             , method = Left GET
                                             , responseFormat = ResponseFormat.json
                                             , headers = [RequestHeader "Authorization" authHeader]
                                             }) 
        pure $ validateRequest res
