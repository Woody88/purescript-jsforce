module Salesforce.Connection.Types where

import Data.FormURLEncoded
import Prelude
import Type.Proxy

import Affjax.RequestBody (RequestBody(..))
import Affjax.ResponseFormat (ResponseFormatError)
import Control.Monad.Except (runExcept)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Semigroup (append)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Foreign.Class (class Encode, class Decode, encode, decode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Salesforce.Types.Common (UserInfo)
import Unsafe.Coerce (unsafeCoerce)

newtype Username    = Username String 
newtype SecretToken = SecretToken String 
newtype ClientId = ClientId String
newtype ClientSecret = Secret String 
newtype SessionId = SessionId String  

data OauthErrorDescription 
    = UnsupportedResponseType 
    | InvalidClientId 
    | InvalidRequest 
    | AccessDenied 
    | RedirectUriMissing 
    | RedirectUriMismatch 
    | ImmediateUnsuccessful
    | InvalidGrant
    | InactiveUser
    | InactiveOrg
    | RateLimitExceeded
    | InvalidScope

type ConnectionError' 
    = ( error             :: String
      , error_description :: Maybe String 
      , state             :: Maybe String
      )

-- type OauthConfig 
--     = ( response_type)

type CommonConfig r 
    = ( username    :: Username 
      , password    :: Password 
      , instanceUrl :: Maybe String 
      , envType     :: EnvironmentType
      , version     :: Number 
      | r
      ) 

type ConnectionAuth' 
    = ( access_token  :: String
      , token_type    :: Maybe String
      , refresh_token :: Maybe String 
      , scope         :: Maybe String
      , state         :: Maybe String 
      , instance_url  :: String
      , id            :: String
      , issued_at     :: String 
      , signature     :: String 
      )

newtype ConnectionAuth = ConnectionAuth { | ConnectionAuth' }

newtype ConnectionError = ConnectionError { | ConnectionError' }

newtype Connection  
    = Connection { userInfo :: UserInfo
                 | ConnectionAuth' 
                 }

data ConnectionConfig
    = SessionOauth { sessionId :: SessionId  
                   , serverUrl :: String   
                   | CommonConfig ()
                   } 
    | UserPassOauth { clientId     :: ClientId
                    , clientSecret :: ClientSecret
                    | CommonConfig ()
                    }
    | WebServerOauth { responseType :: ResponseType 
                     , clientId     :: ClientId
                     , redirectUri  :: String
                     | CommonConfig ()
                     }
    | Soap { | CommonConfig () } 

data RequestError = ResponseDecodeError String | DecodeError String 
data Password = Password String SecretToken 
data EnvironmentType = Production | Sandbox
data GrantType = GPassword | GToken 
data ResponseType = RToken | RCode 

derive instance genericConnectionAuth :: Generic ConnectionAuth _
derive instance genericConnectionError :: Generic ConnectionError _
derive instance genericConnection :: Generic Connection _
derive instance genericRequestError :: Generic RequestError _

derive instance eqRequestError :: Eq RequestError 

instance showConnection :: Show Connection where
  show = genericShow

instance showRequestError :: Show RequestError where
  show = genericShow

instance showConnectionAuth :: Show ConnectionAuth where
  show = genericShow

instance showConnectionError :: Show ConnectionError where
  show = genericShow

instance decodeConnectionAuth :: Decode ConnectionAuth where 
    decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }

instance decodeConnectionError :: Decode ConnectionError where 
    decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }

instance decodeConnection :: Decode Connection where 
    decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }

instance encodeConnection :: Encode Connection where 
    encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }



instance showGrantType :: Show GrantType where
  show GPassword = "password"
  show GToken    = "token"

instance showResponseType :: Show ResponseType where
  show RToken = "token"
  show RCode    = "code"

class ToFormUrlParam a where 
    toFormUrlParam :: a -> Tuple String (Maybe String) 

instance toParamGrantType :: ToFormUrlParam GrantType where
    toFormUrlParam gt = "grant_type" /\ (pure <<< show $ gt)

instance toParamResponseType :: ToFormUrlParam ResponseType where
    toFormUrlParam gt = "response_type" /\ (pure <<< show $ gt)

instance toParamClientId :: ToFormUrlParam ClientId where 
    toFormUrlParam (ClientId i) = "client_id" /\ (pure i)

instance toParamClientSecret :: ToFormUrlParam ClientSecret where 
    toFormUrlParam (Secret s) = "client_secret" /\ (pure s)

instance toParamUsername :: ToFormUrlParam Username where 
    toFormUrlParam (Username u) = "username" /\ (pure u) 

instance toParamPassword :: ToFormUrlParam Password where 
    toFormUrlParam (Password p (SecretToken t)) = "password" /\ (pure <<< append p $ t)