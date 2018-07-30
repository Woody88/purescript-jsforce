module Connection where

import Prelude
import Prim.Row
import Data.Function.Uncurried
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Types 


type ConnectionProps = 
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


data ConnectionType = Browser | CLI 

derive instance genericConnectionType :: Generic ConnectionType _

instance showConnectionType :: Show ConnectionType where
  show = genericShow

connection 
    :: forall props others. Union props others ConnectionProps 
    => PSForce -> { | props } -> Connection
connection psforce props = runFn2 _newConnection psforce props

foreign import _newConnection :: forall props others. Union props others ConnectionProps => Fn2 PSForce { | props } Connection 