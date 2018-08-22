module Salesforce.Util where 

import Prelude ((<>))
import Salesforce.Connection.Types (Connection(..))
import Salesforce.Query.Types
import Salesforce.SObject.Types (SObjectName(..))

data Endpoint r
    = SObject (SObjectName)
    | Queries (QueryEndpoint r)

class BaseUrl a where 
    baseUrl :: forall r. a -> Endpoint r -> String 

instance baseUrlConnection :: BaseUrl Connection where 
    baseUrl (Connection conn) endpoint = case endpoint of 
        SObject (SObjectName sobj)   -> conn.instance_url <> "/services/data/v42.0/sobjects/" <> sobj <> "/"
        Queries (Query _ sep)        -> conn.instance_url <> "/services/data/v42.0/query/?q="
        Queries (QueryExplain _ sep) -> conn.instance_url <> "/services/data/v42.0/query/?explain="
        Queries (QueryReport _ sep)  -> conn.instance_url <> "/services/data/v42.0/query/?explain="
        Queries (NextQuery _ sep)    -> conn.instance_url <> "/services/data/v42.0/query/"
    

