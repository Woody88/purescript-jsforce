module Salesforce.SOQL.Query where

import Prelude ((<<<), ($))
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn4, runFn4)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Salesforce.Types (Connection)

newtype QueryError = QueryError String 

type QueryResult r = 
    { totalSize :: Int
    , done      :: Boolean 
    , records   :: Array (Record r)
    }


queryString :: forall r. Connection -> String -> Aff (Either QueryError (QueryResult r))
queryString conn q = fromEffectFnAff $ runFn4 queryString_ conn q (Left <<< QueryError) Right 


{-- Type Related Issue 
  we the function below there is an assumption that assumption that JSForce provides valid data of type `sobject`.
  And since it’s on the FFI boundary the compiler will trust you.
  Based on Robert this is often referred as the `TypeScript Problem` :)

  With generics, you replace the assumed type on the FFI declaration with `Foreign` and decode it in PureScript, giving it all the guarantees you would have if you had declared the data in PS (edited)
  Generics adds compile-time static type reflection and typeclass-based code generation.

  libraries needed `foreign-generic` + `generics-rep`
--}
foreign import queryString_ :: forall a b c sobject. Fn4 Connection a (String -> b) (QueryResult sobject -> b) (EffectFnAff b)