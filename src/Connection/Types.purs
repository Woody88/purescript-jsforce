module Salesforce.Connection.Types where 

import Prelude

import Data.Maybe (Maybe)

data OrgEnvironment = Production | Sandbox 
newtype Connection = Connection { access_token   :: Token
                                , token_type     :: Maybe String
                                , version        :: Version
                                , orgEnvironment :: OrgEnvironment 
                                }

newtype Token = Token String 
newtype Version = Version Number 

derive instance eqOrgEnvironment :: Eq OrgEnvironment

derive newtype instance showVersion :: Show Version

getToken :: Token -> String
getToken (Token token) = token 