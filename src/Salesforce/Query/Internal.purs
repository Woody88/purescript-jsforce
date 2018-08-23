module Salesforce.Query.Internal where

import Salesforce.Query.Types

import Affjax as AX
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as ResponseFormat
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.Common (replaceAll)
import Data.String.Pattern (Pattern(..), Replacement(..))
import Effect.Class (liftEffect)
import Effect.Console (log)
import Foreign.Class (class Decode)
import Prelude ((<>), show, ($), (#), bind, pure, discard)
import Salesforce.Connection (Connection(..))
import Salesforce.ServerStatus.Internal (decodeWithEitherError)
import Salesforce.Types (Salesforce, salesforce)
import Unsafe.Coerce (unsafeCoerce)

queryUrl :: Maybe Number -> String 
queryUrl Nothing  = "services/data/v42.0/query/"
queryUrl (Just v) = "services/data/v" <> show v <> "/query/"    

query :: forall result. Decode result => SOQL result -> Salesforce QueryError result
query soql = queryRequest $ Query soql "?q=" 

queryRequest :: forall r result. Decode result => QueryEndpoint r -> Salesforce QueryError result 
queryRequest (Query (SOQL soql) sep) = salesforce \(Connection conn) -> do
    res  <- AX.request (AX.defaultRequest { url = url' conn.instance_url
                                          , method = Left GET
                                          , responseFormat = ResponseFormat.string
                                          , headers = [RequestHeader "Authorization" (authHeader conn.access_token $ fromMaybe "" conn.token_type) ]
                                          }) 
    liftEffect $ log $ "Error: " <> show (res.body # lmap AX.printResponseFormatError)
    pure $ decodeWithEitherError res
    where 
        url' bUrl =  bUrl <> "/" <> (queryUrl Nothing) <> sep <> replaceQuerySpace soql
        authHeader token type_ = type_ <> " " <> token
        replaceQuerySpace = \query' -> replaceAll (Pattern " ") (Replacement "+") query'
queryRequest _ = unsafeCoerce "?"
