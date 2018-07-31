module Salesforce.Connection where

import Prelude
import Prim.Row (class Union)
import Data.Function.Uncurried (Fn1)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Salesforce.Types (Connection)


type ConnectionConfig = 
        ( logLevel      :: String
        , version       :: String
        , maxRequest    :: Int
        , loginUrl      :: String
        , instanceUrl   :: String
        , serverUrl     :: String
        , accessToken   :: String
        , sessionId     :: String 
        , refreshToken  :: String
        , signedRequest :: String
        , proxyUrl      :: String 
        )

-- newtype SForce (p :: Platform) e a = SForce (Connection p -> Aff (Either e a))

data ConnectionType = Browser | CLI 

derive instance genericConnectionType :: Generic ConnectionType _

instance showConnectionType :: Show ConnectionType where
  show = genericShow

-- connection 
--     :: forall props others. Union props others ConnectionProps 
--     => { | props } -> Connection p 
-- connection psforce props = runFn1 _newConnection props


foreign import _newConnection :: forall props others. Union props others ConnectionConfig => Fn1 { | props } Connection