module Salesforce.ApprovalProcess.Types where 

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Foreign.Class (class Decode)
import Foreign.Generic (defaultOptions, genericDecode)
import Foreign.Generic.EnumEncoding (defaultGenericEnumOptions, genericDecodeEnum)
import Prelude (class Show, ($))

data ActionType = Submit | Approve | Reject 
data ApprovalStatus = Approved | Rejected | Removed | Pending 

newtype ApprovalProcess 
    = ApprovalProcess { actionType                :: ActionType 
                      , contextActorId            :: String 
                      , contextId                 :: String 
                      , comments                  :: String 
                      , nextApproverIds           :: Array String 
                      , processDefinitionNameOrId :: String 
                      , skipEntryCriteria         :: Boolean 
                      } 

newtype ApprovalRequest 
    = ApprovalRequest { requests :: Array ApprovalProcess }

newtype ApprovalResponse 
    = ApprovalResponse { actorIds       :: Array String 
                       , entityId       :: String 
                       , errors         :: Array String 
                       , instanceId     :: String 
                       , instanceStatus :: ApprovalStatus 
                       , newWorkItemIds :: Array String 
                       , success        :: Boolean
                       } 

derive instance genericActionType :: Generic ActionType _
derive instance genericApprovalStatus :: Generic ApprovalStatus _ 
derive instance genericApprovalProcess :: Generic ApprovalProcess _
derive instance genericApprovalRequest :: Generic ApprovalRequest _
derive instance genericApprovalResponse :: Generic ApprovalResponse _ 

instance decodeActionType :: Decode ActionType where
  decode = genericDecodeEnum $ defaultGenericEnumOptions

instance decodeApprovalStatus :: Decode ApprovalStatus where
  decode = genericDecodeEnum $ defaultGenericEnumOptions

instance decodeApprovalProcess :: Decode ApprovalProcess where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }

instance decodeApprovalRequest :: Decode ApprovalRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }

instance showActionType :: Show ActionType where 
    show = genericShow

instance showApprovalStatus :: Show ApprovalStatus where 
    show = genericShow

instance showApprovalProcess :: Show ApprovalProcess where 
    show = genericShow