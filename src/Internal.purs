module Salesforce.Internal where 

import Prelude
import Data.Argonaut.Core (Json)
import Data.Either (Either)
import Data.Maybe (Maybe)
import Salesforce.Connection.Types (Connection)
import Salesforce.Util (Url)

type NetworkError = { errorCode :: String, message :: String, fields :: Maybe (Array String)}

class HasEndpoint sfapi where 
    endpointUrl :: Connection -> sfapi -> Url

class HasNetwork m sfapi where 
    request :: 
        HasEndpoint sfapi  
        => Applicative m
        => Connection
        -> sfapi  
        -> m (Either NetworkError Json)