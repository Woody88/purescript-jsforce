module Salesforce.Query.Internal where 

import Prelude

import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Bifunctor (lmap)
import Data.Variant (SProxy(..), Variant, inj)
import Salesforce.Internal (NetworkError, request)
import Salesforce.Query.Types (QueryEndpoint(..), QueryError, SOQL)
import Salesforce.Types (SalesforceV, salesforce)
import Type.Row (type (+))


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