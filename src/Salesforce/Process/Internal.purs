module Salesforce.Process.Internal where
  
import Effect (Effect)
import Salesforce.Types (Connection)
import Salesforce.Process.Types 

foreign import mkProcess :: Connection -> Effect Process  
