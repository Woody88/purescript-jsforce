module Salesforce.ApprovalProcess.Internal where 

import Prelude ((<>), ($), (<$>), bind, pure, mempty)
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
import Foreign.Generic (encodeJSON)
import Salesforce.ApprovalProcess.Types (ApprovalProcessError, ApprovalRequest(..), ApprovalProcess(..), ApprovalResponse(..), mapActionTypeToError)
import Salesforce.Connection.Types (Connection(..))
import Salesforce.Types (Salesforce, salesforce)
import Salesforce.Util (Endpoint(..), baseUrl)

execute :: ApprovalProcess -> Salesforce ApprovalProcessError ApprovalResponse
execute ap'@(ApprovalProcess ap) = salesforce \conn'@(Connection conn) -> do
    res <- AX.request $ AX.defaultRequest { url = baseUrl conn' Approvals
                                          , method = Left POST 
                                          , responseFormat = ResponseFormat.json
                                          , content = pure $ string $ encodeJSON $ ApprovalRequest { requests: [ap'] }
                                          , headers = [ ContentType applicationJSON, RequestHeader "Authorization" (authHeader conn.access_token $ fromMaybe "" conn.token_type)]
                                          }
    let eitherApprovalResult = (decodeWithEitherError $ res { body = stringify <$> res.body }) :: (Either ApprovalProcessError ApprovalResponse)
    pure $ do
        apResp'@(ApprovalResponse apResp) <- eitherApprovalResult
        case apResp.success of 
            true  -> pure apResp'
            false -> throwError $ mapActionTypeToError ap.actionType $ joinWith "\n" $ fromMaybe mempty apResp.errors
    where 
        authHeader token type_ = type_ <> " " <> token

