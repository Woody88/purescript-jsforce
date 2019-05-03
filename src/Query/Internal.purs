module Salesforce.Query.Internal where 

import Prelude

import Control.Monad.Reader (class MonadReader)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Bifunctor (lmap)
import Data.Variant (SProxy(..), Variant, inj)
import Effect.Aff.Class (class MonadAff)
import Salesforce.Connection.Types (Connection)
import Salesforce.Internal (class HasNetwork, class HasNetworkType, request)
import Salesforce.Query.Types (QueryEndpoint(..), QueryError, QueryResult, SOQL)
import Salesforce.Types (NetworkError)
import Salesforce.Util (EitherV)
import Type.Row (type (+))

query :: forall sobject r m nt.  
    MonadReader Connection m 
    => MonadAff m
    => HasNetwork m (QueryEndpoint sobject) nt 
    => HasNetworkType m nt
    => DecodeJson sobject 
    => SOQL sobject 
    -> m (EitherV (QueryError + r) (QueryResult sobject))
query soql = do 
    queryRequest $ Query soql

queryExplain :: forall sobject r m nt. 
    MonadReader Connection m 
    => MonadAff m
    => HasNetwork m (QueryEndpoint sobject) nt 
    => HasNetworkType m nt
    => DecodeJson sobject 
    => SOQL sobject 
    -> m (EitherV (QueryError + r) (QueryResult sobject))
queryExplain soql = do 
    queryRequest $ QueryExplain soql

queryNext :: forall sobject r m nt. 
    MonadReader Connection m 
    => MonadAff m
    => HasNetwork m (QueryEndpoint sobject) nt 
    => HasNetworkType m nt
    => DecodeJson sobject 
    => String
    -> m (EitherV (QueryError + r) (QueryResult sobject))
queryNext next = do 
    queryRequest $ QueryNext next

queryRequest :: forall m sobject nt r.
    DecodeJson sobject
    => HasNetwork m (QueryEndpoint sobject) nt 
    => HasNetworkType m nt
    => MonadReader Connection m
    => MonadAff m
    => QueryEndpoint sobject  
    -> m (EitherV (QueryError + r) (QueryResult sobject))
queryRequest q = do 
    json <- lmap queryError <$> request q
    pure $ json >>= (lmap queryParseError <<< decodeJson)

queryError :: forall r. NetworkError -> Variant (queryError :: NetworkError | r) 
queryError = inj (SProxy :: SProxy "queryError") 

queryParseError :: forall r. String -> Variant (queryParseError :: String | r) 
queryParseError = inj (SProxy :: SProxy "queryParseError") 