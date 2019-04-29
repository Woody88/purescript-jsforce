module Salesforce.Internal where 

import Prelude

import Affjax (Response, ResponseFormatError, printResponseFormatError)
import Affjax.StatusCode (StatusCode(..))
import Control.Monad.Error.Class (throwError)
import Control.Plus (empty)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson, (.:), (.:?))
import Data.Bifunctor (lmap)
import Data.Either (Either, either)
import Data.Maybe (Maybe)
import Salesforce.Connection.Types (Connection)
import Salesforce.Util (Url)

foreign import kind NetworkType  

foreign import data Affjax :: NetworkType 

data NTProxy (n :: NetworkType) = NTProxy 

affjaxNetwork :: NTProxy Affjax 
affjaxNetwork = NTProxy

type NetworkError = { errorCode :: String, message :: String, fields :: Maybe (Array String)}

class HasEndpoint sfapi where 
    endpointUrl :: Connection -> sfapi -> Url

class HasNetwork m sfapi (n :: NetworkType) where 
    request :: 
        HasEndpoint sfapi  
        => Applicative m
        => NTProxy n
        -> Connection
        -> sfapi  
        -> m (Either NetworkError Json)

networkParseError :: String -> NetworkError
networkParseError message = {errorCode: "Network Parse Error", fields: empty, message} 

validateRequest :: Response (Either ResponseFormatError Json) -> Either NetworkError Json 
validateRequest {status: (StatusCode status), body} 
    | status >= 200 && status < 304 = either (throwError <<< networkParseError) (pure <<< identity) $ lmap printResponseFormatError body 
    | otherwise                     = throwError $ either (networkParseError <<< printResponseFormatError) decodeNetworkError body

decodeNetworkError :: Json -> NetworkError
decodeNetworkError json = do 
    let eitherNetworkErr = do
            obj       <- decodeJson json
            errorCode <- obj .: "errorCode"
            message   <- obj .: "message"
            fields    <- obj .:? "fields"
            (pure {errorCode, message, fields} :: Either String NetworkError)
    either networkParseError identity eitherNetworkErr