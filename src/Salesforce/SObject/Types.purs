module Salesforce.SObject.Types where 

import Data.Maybe (Maybe)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Foreign.Class (class Decode)
import Foreign.Generic (defaultOptions, genericDecode)
import Prelude (class Show, ($))

data SObjectError 
    = InsertError String
    | SObjectError String 
    | SObjectParseError String 

type InsertResults = Array InsertResult

newtype InsertResult 
    = InsertResult { id      :: String 
                   , errors  :: Maybe (Array String) 
                   , success :: Boolean
                   } 

newtype SObjectId = SObjectId String 

newtype SObjectName = SObjectName String 

derive instance genericSObjectId :: Generic SObjectId _ 
derive instance genericSObjectError :: Generic SObjectError _ 
derive instance genericInsertResult :: Generic InsertResult _ 

instance decodeSObjectId :: Decode SObjectId where
    decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }

instance decodeInsertResult :: Decode InsertResult where
    decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }

instance showSObjectError :: Show SObjectError where 
    show = genericShow

instance showInsertResult :: Show InsertResult where 
    show = genericShow

instance showSObjectId :: Show SObjectId where 
    show = genericShow

sobjectName  :: String -> SObjectName
sobjectName = SObjectName

sobjectId :: String -> SObjectId
sobjectId = SObjectId 