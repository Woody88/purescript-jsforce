module Salesforce.Connection where

import Prelude

import Data.Either (Either(..))
import Data.Tuple
import Data.Function.Uncurried (Fn1)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Function.Uncurried (Fn6, runFn6)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Prim.Row (class Union)
import Salesforce.Types (Connection, Username(..), Password(..))
import Salesforce.SObject.Types (UserInfo)

data LoginError = LoginError String

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
        , clientId      :: String
        , redirectUri   :: String
        , proxyUrl      :: String 
        )

-- newtype SForce (p :: Platform) e a = SForce (Connection p -> Aff (Either e a))

data ConnectionType = Browser | CLI 

derive instance genericConnectionType :: Generic ConnectionType _

instance showConnectionType :: Show ConnectionType where
  show = genericShow

mkConnection 
    :: forall configs others. Union configs others ConnectionConfig 
    => { | configs } -> Effect Connection 
mkConnection configs = mkConnection_ configs

login :: Connection -> Username -> Password -> Aff (Either LoginError (Tuple Connection UserInfo))
login conn (Username u) (Password p s) = fromEffectFnAff $ runFn6 login_ conn u (p <> s) (Left <<< LoginError) Tuple Right

foreign import login_ :: forall a b. Fn6 Connection String String (String -> a) (Connection -> UserInfo -> b) (b -> a) (EffectFnAff a)

foreign import mkConnection_ :: forall configs others. Union configs others ConnectionConfig => { | configs } -> Effect Connection