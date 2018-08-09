module Salesforce.Process.Types where 

foreign import data Process :: Type 

newtype ProcessRuleDefinition 
    = ProcessRuleDefinition { id     :: String
                            , name   :: String 
                            , object :: String 
                            } 

newtype ProcessRuleTriggerResult 
    = ProcessRuleTriggerResult { id     :: String
                               , name   :: String 
                               , object :: String 
                               } 
