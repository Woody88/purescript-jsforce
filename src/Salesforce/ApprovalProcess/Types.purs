module Salesforce.ApprovalProcess.Types where 

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Foreign.Generic.EnumEncoding (defaultGenericEnumOptions, genericDecodeEnum, genericEncodeEnum)
import Prelude (class Show, ($))

data ApprovalProcessError 
    = ApprovalProcessParseError String 
    | ApprovalProcessError String 
    | SubmitError String
    | RejectError String
    | ApprovalError String 

data ActionType = Submit | Approve | Reject 
data ApprovalStatus = Approved | Rejected | Removed | Pending 

newtype ApprovalProcess 
    = ApprovalProcess { actionType                :: ActionType 
                      , contextActorId            :: String 
                      , contextId                 :: String 
                      , comments                  :: Maybe String 
                      , nextApproverIds           :: Maybe (Array String) 
                      , processDefinitionNameOrId :: String 
                      , skipEntryCriteria         :: Boolean 
                      } 

newtype ApprovalRequest 
    = ApprovalRequest { requests :: Array ApprovalProcess }

newtype ApprovalResponse 
    = ApprovalResponse { actorIds       :: Array String 
                       , entityId       :: String 
                       , errors         :: Maybe (Array String) 
                       , instanceId     :: String 
                       , instanceStatus :: ApprovalStatus 
                       , newWorkItemIds :: Array String 
                       , success        :: Boolean
                       } 

derive instance genericActionType :: Generic ActionType _
derive instance genericApprovalStatus :: Generic ApprovalStatus _ 
derive instance genericApprovalProcess :: Generic ApprovalProcess _
derive instance genericApprovalProcessError :: Generic ApprovalProcessError _
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

instance decodeApprovalResponse :: Decode ApprovalResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }

{-- Encode --}
instance encodeActionType :: Encode ActionType where
  encode = genericEncodeEnum $ defaultGenericEnumOptions

instance encodeApprovalStatus :: Encode ApprovalStatus where
  encode = genericEncodeEnum $ defaultGenericEnumOptions

instance encodeApprovalProcess :: Encode ApprovalProcess where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }

instance encodeApprovalRequest :: Encode ApprovalRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


instance showActionType :: Show ActionType where 
    show = genericShow

instance showApprovalStatus :: Show ApprovalStatus where 
    show = genericShow

instance showApprovalProcess :: Show ApprovalProcess where 
    show = genericShow

instance showApprovalResponse :: Show ApprovalResponse where 
    show = genericShow

instance showApprovalProcessError :: Show ApprovalProcessError where 
    show = genericShow

mapActionTypeToError :: ActionType -> (String -> ApprovalProcessError)
mapActionTypeToError at = case at of 
    Submit   ->  SubmitError
    Approve  ->  ApprovalError
    Reject   ->  RejectError
