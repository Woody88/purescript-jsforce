module Salesforce.Connection.CRUD where

import Prelude

import Control.Monad.Error.Class (catchError, throwError)
import Control.Monad.Except (runExcept)
import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn5, runFn5)
import Data.Newtype (class Newtype, unwrap, wrap)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Foreign (Foreign, MultipleErrors)
import Foreign.Class (class Decode, decode)
import Foreign.Generic (genericDecode, decodeJSON, defaultOptions)
import Prim.Row (class Lacks, class Union)
import Salesforce.Types (RecordResult, Connection, SFId(..), SObjectName(..), Salesforce, salesforce, decodeErrorParser)
import Simple.JSON as JSON

data RequestError  
    = DecodeError MultipleErrors
    | Error String 
    | SaveFailed (Array String)

type SFDCId r = { id :: String | r}

derive instance genericRequestError :: Generic RequestError _

instance showRequestError :: Show RequestError where
  show = genericShow


retrieve' :: forall a. JSON.ReadForeign a => SFId -> Salesforce RequestError a
retrieve' (SFId rid (SObjectName type_)) = salesforce \conn -> do
    eitherF <- runRetrieve conn type_ rid 
    pure $ eitherF >>= (\x -> decodeErrorParser DecodeError $ JSON.read x)

retrieve :: forall a. Decode a => SFId -> Salesforce RequestError a
retrieve (SFId rid (SObjectName type_)) = salesforce \conn -> do
    eitherF <- runRetrieve conn type_ rid 
    pure $ eitherF >>= (\x -> decodeErrorParser DecodeError <<< runExcept <<< decode $ x)

update :: forall result record. JSON.ReadForeign result => Newtype result { "Id" :: String | record } => SObjectName -> result -> Salesforce RequestError RecordResult
update (SObjectName type_) rec = salesforce \conn -> do
    eitherF <- runUpdate conn type_ $ unwrap rec 
    pure $ eitherF >>= decodeRecordResult 

create :: forall result record. JSON.ReadForeign result => Newtype result { "Id" :: String | record } => SObjectName -> result -> Salesforce RequestError result 
create (SObjectName type_) rec = salesforce \conn -> do
  eitherF <- runCreate conn type_ $ unwrap rec 
  pure $ eitherF >>= processData
  where 
    processData x = returnId (unwrap rec) =<< decodeRecordResult x 
    returnId r {success: true, id: id_ } = pure $ wrap $ r { "Id" = id_ }
    returnId _ {success: false, errors: errs } = throwError $ SaveFailed errs 

destroy :: SFId -> Salesforce RequestError RecordResult
destroy (SFId rid (SObjectName type_)) = salesforce \conn -> do
    eitherDestroy <- runDestroy conn type_ rid 
    pure $ eitherDestroy >>= decodeRecordResult

decodeRecordResult :: Foreign -> Either RequestError RecordResult 
decodeRecordResult = bimap DecodeError identity <<< runExcept <<< JSON.read'   

runRetrieve :: Connection -> String -> String -> Aff (Either RequestError Foreign)
runRetrieve conn type_ id = fromEffectFnAff $ runFn5 retrieve_ conn type_ id (Left <<< Error) Right 

runUpdate :: forall record. Connection -> String -> { "Id" :: String | record } -> Aff (Either RequestError Foreign)
runUpdate conn type_ rec = fromEffectFnAff $ runFn5 update_ conn type_ rec (Left <<< Error) Right

runCreate :: forall record. Connection -> String -> { | record } -> Aff (Either RequestError Foreign)
runCreate conn type_ rec = fromEffectFnAff $ runFn5 create_ conn type_ rec (Left <<< Error) Right

runDestroy :: Connection -> String -> String -> Aff (Either RequestError Foreign)
runDestroy conn type_ id = fromEffectFnAff $ runFn5 destroy_ conn type_ id (Left <<< Error) Right 

foreign import retrieve_ :: forall a b. Fn5 Connection a a (a -> b) (Foreign -> b) (EffectFnAff b)

foreign import update_ :: forall a b rec. Fn5 Connection a {"Id":: String | rec} (a -> b) (Foreign -> b) (EffectFnAff b)

foreign import create_ :: forall a b rec. Fn5 Connection a rec (a -> b) (Foreign -> b) (EffectFnAff b) 

foreign import destroy_ :: forall a b. Fn5 Connection a a (a -> b) (Foreign -> b) (EffectFnAff b)