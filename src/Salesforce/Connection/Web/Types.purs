module Salesforce.Connection.Web.Types where 

import Prelude (class Show)

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Foreign.Class (class Decode)
import Foreign.Generic (defaultOptions, genericDecode)
import Salesforce.Connection.Types

data OauthResponse 
    = OauthConnectionError ConnectionError 
    | OauthConnection ConnectionAuth

data OauthError 
    = DecodeError String 
    | Error String 

derive instance genericOauthResponse :: Generic OauthResponse _

derive instance genericOauthError :: Generic OauthError _


instance showOauthError :: Show OauthError where
  show = genericShow

instance decodeOauthResponse :: Decode OauthResponse where
    decode = genericDecode defaultOptions