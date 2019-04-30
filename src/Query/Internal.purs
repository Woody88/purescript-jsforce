module Salesforce.Query.Internal where 

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Variant (SProxy(..), Variant, inj)
import Effect.Aff (Aff)
import Salesforce.Connection.Types (Connection)
import Salesforce.Internal (class HasNetwork, NTProxy, NetworkError, affjaxNetwork, request)
import Salesforce.Query.Types (QueryEndpoint(..), QueryError, SOQL)
import Salesforce.Types (SalesforceV, salesforce)
import Type.Row (type (+))

query :: forall sobject r. DecodeJson sobject => SOQL sobject -> SalesforceV (QueryError + r) sobject
query soql = salesforce \conn -> do 
    res <- (lmap queryError) <$> (queryRequest affjaxNetwork conn $ Query soql) 
    pure $ res >>= lmap queryParseError <<< decodeJson

queryExplain :: forall sobject r. DecodeJson sobject => SOQL sobject -> SalesforceV (QueryError + r) sobject
queryExplain soql = salesforce \conn -> do 
    res <- (lmap queryError) <$> (request affjaxNetwork conn $ QueryExplain soql) 
    pure $ res >>= lmap queryParseError <<< decodeJson 

queryRequest :: forall sobject m nt. 
    Applicative m
    => HasNetwork m (QueryEndpoint sobject) nt 
    => NTProxy nt 
    -> Connection
    -> QueryEndpoint sobject  
    -> m (Either NetworkError Json)
queryRequest nt conn q = request nt conn q 
    
queryError :: forall r. NetworkError -> Variant (queryError :: NetworkError | r) 
queryError = inj (SProxy :: SProxy "queryError") 

queryParseError :: forall r. String -> Variant (queryParseError :: String | r) 
queryParseError = inj (SProxy :: SProxy "queryParseError") 