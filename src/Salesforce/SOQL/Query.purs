module Salesforce.SOQL.Query where

import Control.Monad.Error.Class
import Control.Monad.Except
import Prelude

import Data.Either (Either(..), either)
import Data.Function.Uncurried (Fn4, runFn4)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Effect.Console (log)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Effect.Class (liftEffect)
import Foreign (Foreign, MultipleErrors)
import Foreign.Class (class Decode, decode)
import Foreign.Generic (genericDecode, decodeJSON, defaultOptions)
import Salesforce.Types (runSalesforce, Connection, Salesforce, SalesforceM(..))

newtype SOQL result = SOQL String 

data QueryError = QueryError String | SOQLParseError MultipleErrors 

type QueryResult r = 
    { totalSize :: Int
    , done      :: Boolean 
    , records   :: Array (Record r)
    }
    
newtype SOQLResult r = SOQLResult
  { totalSize :: Int
  , done      :: Boolean 
  , records   :: r
  }

derive instance genericeQueryError :: Generic QueryError _ 
derive instance genericSOQLResult :: Generic (SOQLResult r) _
-- derive instance genericSOQLResult' :: Generic SOQLResult' _

instance decodeSOQLResult :: Decode r => Decode (SOQLResult r) where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}

instance showQueryError :: Show QueryError where
  show = genericShow

queryString :: forall r. Connection -> String -> Aff (Either QueryError (QueryResult r))
queryString conn q = fromEffectFnAff $ runFn4 queryString_ conn q (Left <<< QueryError) Right 

query :: forall result. Decode result => SOQL result -> Salesforce QueryError result
query soql = ExceptT $ SalesforceM \conn -> do
  liftEffect $ log "executing..."
  let eith = runExceptT $ query' soql 
  runSalesforce eith conn 
    >>= \eitherF -> 
      pure $ eitherF >>= \(SOQLResult res) -> pure $ res.records
  -- liftEffect $ either (\x -> log "error") (\x -> log "hello world") (Right 1)

query' :: forall result. Decode result => SOQL result -> Salesforce QueryError (SOQLResult result)
query' soql = ExceptT $ SalesforceM \conn -> do
  liftEffect $ log "executing..."
  runSOQL conn soql
    >>= \eitherF -> 
      pure $ eitherF >>= \x -> decodeErrorParser <<< runExcept <<< decode $ x
  -- liftEffect $ either (\x -> log "error") (\x -> log "hello world") a


decodeErrorParser :: forall a. Decode a => Either MultipleErrors a -> Either QueryError a
decodeErrorParser (Left x) = throwError $ SOQLParseError x
decodeErrorParser (Right x) = pure x

runSOQL :: forall a. Connection -> SOQL a -> Aff (Either QueryError Foreign)
runSOQL conn (SOQL q) = fromEffectFnAff $ runFn4 runSOQL_ conn q (Left <<< QueryError) Right

{-- Type Related Issue 
  we the function below there is an assumption that assumption that JSForce provides valid data of type `sobject`.
  And since it’s on the FFI boundary the compiler will trust you.
  Based on Robert this is often referred as the `TypeScript Problem` :)

  With generics, you replace the assumed type on the FFI declaration with `Foreign` and decode it in PureScript, giving it all the guarantees you would have if you had declared the data in PS (edited)
  Generics adds compile-time static type reflection and typeclass-based code generation.

  libraries needed `foreign-generic` + `generics-rep`
--}
foreign import queryString_ :: forall a b c sobject. Fn4 Connection a (String -> b) (QueryResult sobject -> b) (EffectFnAff b)


foreign import runSOQL_ :: forall a b. Fn4 Connection a (String -> b) (Foreign -> b) (EffectFnAff b)
