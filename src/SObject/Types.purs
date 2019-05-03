module Salesforce.SObject.Types where 

newtype SObjectId sobject = SObjectId String 

data SObjectEndpoint sobject = Insert sobject | Update sobject | Delete (SObjectId sobject)

class HasSObjectName sobject (name :: Symbol) 