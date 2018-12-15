module Salesforce.Connection.Web.Types where 

import Salesforce.Connection.Types

import Data.Argonaut.Core (Json)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map as Map
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Foreign.Object (Object)
import Prelude (class Show, class Eq, show)

newtype RouteQuery a = RouteQuery a

data OauthResponse 
    = OauthConnectionError { | ConnectionError' }
    | OauthConnection { | ConnectionAuth' }

data OauthError 
    = DecodeError String 
    | Error String 

derive instance genericRouteQuerye :: Generic (RouteQuery a) _

derive instance genericOauthResponse :: Generic OauthResponse _

derive instance genericOauthError :: Generic OauthError _

derive instance eqOauthResponse :: Eq OauthResponse 

derive instance eqOauthError :: Eq OauthError

derive newtype instance eqRouteQuery :: Eq a => Eq (RouteQuery a) 



instance showRouteQuery :: Show a => Show (RouteQuery a) where
    show (RouteQuery q) = show q

instance showOauthError :: Show OauthError where
    show = genericShow

instance showOauthResponse :: Show OauthResponse where
    show = genericShow


instance decodeOauthError :: Decode OauthError where
    decode = genericDecode defaultOptions

instance encodeOauthError :: Encode OauthError where
    encode = genericEncode defaultOptions

instance decodeOauthResponse :: Decode OauthResponse where
    decode = genericDecode defaultOptions

instance encodeOauthResponse :: Encode OauthResponse where
    encode = genericEncode defaultOptions
