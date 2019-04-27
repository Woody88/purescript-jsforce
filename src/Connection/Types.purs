module Salesforce.Connection.Types where 

import Prelude

data OrgEnvironment = Production | Sandbox 
newtype Connection = Connection { access_token :: Token, version :: Version, orgEnvironment :: OrgEnvironment }
newtype Token = Token String 
newtype Version = Version Number 

derive instance eqOrgEnvironment :: Eq OrgEnvironment

derive newtype instance showVersion :: Show Version