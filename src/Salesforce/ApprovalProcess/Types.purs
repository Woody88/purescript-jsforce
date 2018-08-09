module Salesforce.ApprovalProcess.Types where

import Prelude

import Data.Generic.Rep (class Generic)
import Foreign.Generic (genericDecode, genericEncode, decodeJSON, genericEncodeJSON, encodeJSON, defaultOptions)
import Simple.JSON as JSON

foreign import data ApprovalProcess :: Type 

newtype Comments = Comments  String 
newtype ContextId = ContextId String 


newtype ApprovalProcessDefinition 
    = ApprovalProcessDefinition { id        :: String
                                , name      :: String 
                                , object    :: String 
                                , sortOrder :: Int 
                                } 

newtype ApprovalProcessRequestResult 
    = ApprovalProcessRequestResult { success        :: Boolean
                                   , errors         :: Array String
                                   , actorIds       :: String 
                                   , entityId       :: Int 
                                   , instanceId     :: String
                                   , instanceStatus :: String 
                                   , newWorkItemIds :: Array String 
                                   } 

type ApprovalOptions = ( nextApproverIds           :: Array String
                       , processDefinitionNameOrId :: String 
                       , skipEntryCriteria         :: Boolean
                       )

derive instance genericAccount :: Generic (ApprovalProcessRequestResult) _

instance readForeignApprovalProcessRequestResult :: JSON.ReadForeign ApprovalProcessRequestResult where 
  readImpl = genericDecode $ defaultOptions {unwrapSingleConstructors = true}

instance writeForeignApprovalProcessRequestResult :: JSON.WriteForeign ApprovalProcessRequestResult where
  writeImpl = genericEncode $ defaultOptions {unwrapSingleConstructors = true}
