module Salesforce.SObject.Internal where

import Affjax as AX
import Salesforce.ServerStatus.Internal (decodeWithEitherError)
import Affjax.RequestBody (string)
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.Except.Trans (throwError)
import Data.Argonaut.Core (stringify)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (fromMaybe)
import Data.MediaType.Common (applicationJSON)
import Data.String (joinWith)
import Foreign.Class (class Encode)
import Foreign.Generic (encodeJSON)
import Prelude ((<$>), (<>), ($), bind, pure, mempty, Unit)
import Salesforce.Connection.Types (Connection(..))
import Salesforce.SObject.Types (SObjectError(..), SObjectId(..), SObjectName, InsertResult(..), sobjectId)
import Salesforce.Types (Salesforce, salesforce)
import Salesforce.Util (Endpoint(..), baseUrl)

insert :: forall a. Encode a => SObjectName -> a -> Salesforce SObjectError SObjectId
insert sobjName sobj = salesforce \conn'@(Connection conn) -> do
    res <- AX.request $ AX.defaultRequest { url = baseUrl conn' $ SObject sobjName
                                          , method = Left POST 
                                          , responseFormat = ResponseFormat.json
                                          , content = pure $ string $ encodeJSON sobj
                                          , headers = [ ContentType applicationJSON, RequestHeader "Authorization" (authHeader conn.access_token $ fromMaybe "" conn.token_type)]
                                          }
    let eitherInsertResult = (decodeWithEitherError $ res { body = stringify <$> res.body }) :: (Either SObjectError InsertResult)
    pure $ do
        (InsertResult insertResult) <- eitherInsertResult
        case insertResult.success of 
            true  -> pure $ sobjectId insertResult.id
            false -> throwError $ InsertError $ joinWith "\n" $ fromMaybe mempty insertResult.errors
    where 
        authHeader token type_ = type_ <> " " <> token


update :: forall a. Encode a => SObjectName -> SObjectId -> a -> Salesforce SObjectError Unit 
update sobjName (SObjectId sobjId) sobj = salesforce \conn'@(Connection conn) -> do
    res <- AX.request $ AX.defaultRequest { url = (baseUrl conn' $ SObject sobjName) <> sobjId
                                              , method = Left PATCH 
                                              , responseFormat = ResponseFormat.json
                                              , content = pure $ string $ encodeJSON sobj
                                              , headers = [ ContentType applicationJSON, RequestHeader "Authorization" (authHeader conn.access_token $ fromMaybe "" conn.token_type)]
                                              }
    pure $ decodeWithEitherError $ res { body = stringify <$> res.body }
    where 
        authHeader token type_ = type_ <> " " <> token

delete :: SObjectName -> SObjectId -> Salesforce SObjectError Unit 
delete sobjName (SObjectId sobjId) = salesforce \conn'@(Connection conn) -> do
    res <- AX.request $ AX.defaultRequest { url = (baseUrl conn' $ SObject sobjName) <> sobjId
                                          , method = Left DELETE 
                                          , responseFormat = ResponseFormat.json
                                          , headers = [ RequestHeader "Authorization" (authHeader conn.access_token $ fromMaybe "" conn.token_type)]
                                          }
    pure $ decodeWithEitherError $ res { body = stringify <$> res.body }
    where 
        authHeader token type_ = type_ <> " " <> token
