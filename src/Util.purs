module Salesforce.Util where 

import Data.Either (Either)
import Data.Variant (Variant)
type Url = String 

type EitherV e = Either (Variant e) 