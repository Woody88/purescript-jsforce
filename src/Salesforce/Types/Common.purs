module Salesforce.Types.Common where

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Foreign.Class (class Decode, decode)
import Foreign.Generic (defaultOptions, genericDecode)
import Prelude (class Show, ($))

newtype UserInfo 
    = UserInfo { userId :: String 
               , orgId  :: String 
               , url    :: String 
               }

derive instance genericUserInfo :: Generic UserInfo _ 

instance decodeUserInfo :: Decode UserInfo where 
    decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }

instance showUserInfo :: Show UserInfo where 
    show = genericShow 