module Salesforce.ApprovalProcess.Internal where

import Prelude
import Salesforce.ApprovalProcess.Types

import Control.Monad.Except (runExcept)
import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn6, runFn6)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Effect.Class (liftEffect)
import Foreign (Foreign)
import Prim.Row (class Union)
import Salesforce.Connection (RequestError(..))
import Salesforce.Types (Connection, Salesforce, salesforce)
import Simple.JSON as JSON


submit :: forall options o. Union options o ApprovalOptions 
    => ContextId -> Comments -> { | options } -> Salesforce RequestError ApprovalProcessRequestResult
submit (ContextId cntxId) (Comments cmnts) opts = salesforce \conn -> do
    apvp <- liftEffect $ mkApprovalProcess conn 
    eitherF <- runSubmit apvp cntxId cmnts opts 
    pure $ eitherF >>= decodeRecordResult

runSubmit :: forall options o. Union options o ApprovalOptions => ApprovalProcess ->  String -> String -> { | options } -> Aff (Either RequestError Foreign)
runSubmit apvp cntxId cmnts opts = fromEffectFnAff $ runFn6 submitImpl apvp cntxId cmnts opts (Left <<< Error) Right

decodeRecordResult :: Foreign -> Either RequestError ApprovalProcessRequestResult
decodeRecordResult = bimap DecodeError identity <<< runExcept <<< JSON.read'   

foreign import submitImpl :: forall a b o options. Union options o ApprovalOptions => Fn6 ApprovalProcess a a { | options } (a -> b) (Foreign -> b) (EffectFnAff b)
foreign import mkApprovalProcess :: Connection -> Effect ApprovalProcess 
