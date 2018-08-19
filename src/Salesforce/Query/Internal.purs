module Query.Internal where

import Data.String.Common
import Query.Types

import Affjax as AX
import Affjax.RequestBody (formURLEncoded, string)
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as ResponseFormat
import Connection (Connection(..))
import Control.Monad.Except (runExcept)
import Data.Argonaut.Core (Json, stringify)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.Pattern (Pattern(..), Replacement(..))
import Effect.Class (liftEffect)
import Effect.Console (log)
import Foreign.Class (class Decode, decode)
import Foreign.JSON (decodeJSONWith)
import Salesforce.Types (Salesforce, salesforce, runSalesforceT)
import Prelude ((<<<), (<>), show, ($), (#), flip, bind, pure, identity, discard)
import Unsafe.Coerce (unsafeCoerce)

queryUrl :: Maybe Number -> String 
queryUrl Nothing  = "services/data/v42.0/query/"
queryUrl (Just v) = "services/data/v" <> show v <> "/query/"    

query :: forall result. Decode result => SOQL result -> Salesforce QueryError result
query soql = do
    json <- queryRequest $ Query soql "?q=" 
    liftEffect $ log $ stringify json
    salesforce \_ ->  pure $ (runExcept $ runDecoder $ stringify json) # handleDecodeError

queryRequest :: forall r. QueryEndpoint r -> Salesforce QueryError Json 
queryRequest (Query (SOQL soql) sep) = salesforce \(Connection conn) -> do
    res  <- AX.request (AX.defaultRequest { url = url' conn.instance_url
                                          , method = Left GET
                                          , responseFormat = ResponseFormat.json
                                          , headers = [RequestHeader "Authorization" (authHeader conn.access_token $ fromMaybe "" conn.token_type) ]
                                          })  
    pure $ res.body # handleResponseError
    where 
        url' bUrl =  bUrl <> "/" <> (queryUrl Nothing) <> sep <> replaceQuerySpace soql
        authHeader token type_ = type_ <> " " <> token
        replaceQuerySpace = \query -> replaceAll (Pattern " ") (Replacement "+") query
queryRequest _ = unsafeCoerce "?"

handleResponseError = lmap (\err -> QueryError $ AX.printResponseFormatError err)
handleDecodeError = lmap (\err -> QueryParseError $ show err)

runDecoder = decodeJSONWith decode