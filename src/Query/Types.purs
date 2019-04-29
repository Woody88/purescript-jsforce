module Salesforce.Query.Types where 

import Prelude

import Affjax as AX
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as ResponseFormat
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Effect.Aff (Aff)
import Salesforce.Connection.Util (baseUrl, authorizationHeader)
import Salesforce.Internal (class HasEndpoint, class HasNetwork, Affjax, NetworkError, endpointUrl, validateRequest, kind NetworkType)

newtype SOQL r = SOQL String  

data QueryEndpoint r = Query (SOQL r) | QueryExplain (SOQL r)

type QueryError r = ( queryError      :: NetworkError
                    , queryParseError :: String 
                    | r
                    )

instance hasQueryEndpoint :: HasEndpoint (QueryEndpoint r) where 
    endpointUrl conn queryEndpoint = do
        let formatSoqlToUrlParams = \soql -> String.replaceAll (Pattern " ") (Replacement "+") soql

        case queryEndpoint of 
            Query (SOQL soql)        -> baseUrl conn <> "/query/?q=" <> formatSoqlToUrlParams soql 
            QueryExplain (SOQL soql) -> baseUrl conn <> "/query/?explain=" <> formatSoqlToUrlParams soql 

instance hasQueryNetwork :: HasNetwork Aff (QueryEndpoint r) Affjax where 
    request _ conn queryEndpoint = do 
        let url        = endpointUrl conn queryEndpoint 
            authHeader = authorizationHeader conn 
        res <- AX.request (AX.defaultRequest { url = url
                                             , method = Left GET
                                             , responseFormat = ResponseFormat.json
                                             , headers = [RequestHeader "Authorization" authHeader]
                                             }) 
        pure $ validateRequest res
