module Salesforce.Types.Common where

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Foreign.Class (class Decode, class Encode, decode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Prelude (class Show, class Eq, ($))

newtype UserInfo 
    = UserInfo { userId :: String 
               , orgId  :: String 
               , url    :: String 
               }

derive instance genericUserInfo :: Generic UserInfo _ 

derive newtype instance eqUserInfo :: Eq UserInfo 

instance decodeUserInfo :: Decode UserInfo where 
    decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }

instance encodeUserInfo :: Encode UserInfo where 
    encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


instance showUserInfo :: Show UserInfo where 
    show = genericShow 