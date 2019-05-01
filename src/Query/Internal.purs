module Salesforce.Query.Internal where 

import Prelude

import Control.Monad.Reader (class MonadReader)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Bifunctor (lmap)
import Data.Variant (SProxy(..), Variant, inj)
import Effect.Aff.Class (class MonadAff)
import Salesforce.Connection.Types (Connection)
import Salesforce.Internal (class HasNetwork, affjaxNetwork, request)
import Salesforce.Query.Types (QueryEndpoint(..), QueryError, QueryResult, SOQL)
import Salesforce.Types (NTProxy, NetworkError)
import Salesforce.Util (EitherV)
import Type.Row (type (+))

query :: forall sobject r m. 
    MonadReader Connection m 
    => MonadAff m
    => DecodeJson sobject 
    => SOQL sobject 
    -> m (EitherV (QueryError + r) (QueryResult sobject))
query soql = do 
    queryRequest affjaxNetwork $ Query soql

queryExplain :: forall sobject r m. 
    MonadReader Connection m 
    => MonadAff m
    => DecodeJson sobject 
    => SOQL sobject 
    -> m (EitherV (QueryError + r) (QueryResult sobject))
queryExplain soql = do 
    queryRequest affjaxNetwork $ QueryExplain soql

queryRequest :: forall m sobject nt r.
    Applicative m 
    => DecodeJson sobject
    => HasNetwork m (QueryEndpoint sobject) nt 
    => MonadReader Connection m
    => MonadAff m
    => NTProxy nt 
    -> QueryEndpoint sobject  
    -> m (EitherV (QueryError + r) (QueryResult sobject))
queryRequest nt q = do 
    json <- lmap queryError <$> request nt q
    pure $ json >>= (lmap queryParseError <<< decodeJson)

queryError :: forall r. NetworkError -> Variant (queryError :: NetworkError | r) 
queryError = inj (SProxy :: SProxy "queryError") 

queryParseError :: forall r. String -> Variant (queryParseError :: String | r) 
queryParseError = inj (SProxy :: SProxy "queryParseError") 