module Salesforce.Types where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (ExceptT(..), runExceptT)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Foreign (MultipleErrors)
import Foreign.Class (class Decode)
import Foreign.Generic (defaultOptions, genericDecode)
import Salesforce.ApprovalProcess.Types (ApprovalProcessError(..))
import Salesforce.Connection.Types (Connection)
import Salesforce.Query.Types (QueryError)
import Salesforce.SObject.Types (SObjectError)

data SalesforceError 
  = SObjectErr SObjectError 
  | QueryErr QueryError
  | ApprovalProcessErr ApprovalProcessError
  | UnknownErr String

derive instance genericSalesforceError :: Generic SalesforceError _ 

instance showSalesforceError :: Show SalesforceError where 
  show = genericShow 

data SFEndpoint 
  = SObject
  | ApprovalProcess
  | Query

type SalesforceErrorResponses = Array SalesforceErrorResponse

newtype SalesforceErrorResponse 
  = SFErrorResponse { fields    :: Maybe (Array String) 
                    , message   :: String
                    , errorCode :: String 
                    }

derive instance genericSalesforceErrorResponse :: Generic SalesforceErrorResponse _ 

instance decodeSalesforceErrorResponse :: Decode SalesforceErrorResponse where 
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }

newtype SalesforceM a = SalesforceM (Connection -> Aff a)

type Salesforce e a = ExceptT e SalesforceM a

instance functorSalesforceM :: Functor SalesforceM where
  map f (SalesforceM s) = SalesforceM \c -> f <$> s c  

instance applySalesforceM:: Apply SalesforceM where
  apply (SalesforceM f) (SalesforceM s) = SalesforceM \c -> f c <*> s c 

instance applicativeSalesforceM :: Applicative SalesforceM where 
  pure x = SalesforceM \c -> pure x

instance bindSalesforceM :: Bind SalesforceM where
  bind (SalesforceM s) f = SalesforceM \c -> do 
    a <- s c
    let (SalesforceM g) = f a
    g c

instance monadSalesforceM :: Monad SalesforceM  

instance monadEffectSalesforceM :: MonadEffect SalesforceM where
  liftEffect a = SalesforceM \_ -> liftEffect a

instance monadAffectSalesforceM :: MonadAff SalesforceM where
  liftAff a = SalesforceM \_ -> a

runSalesforce :: forall a. SalesforceM a -> Connection -> Aff a
runSalesforce (SalesforceM f) conn =
  f conn

salesforce :: forall e a. (Connection -> Aff (Either e a)) -> Salesforce e a
salesforce = ExceptT <<< SalesforceM

runSalesforceT :: forall e a. Salesforce e a -> Connection -> Aff (Either e a)
runSalesforceT s conn = do
  let (SalesforceM f) = runExceptT s
  f conn

decodeErrorParser :: forall a error. (MultipleErrors -> error) -> Either MultipleErrors a -> Either error a
decodeErrorParser error (Left x) = throwError $ error x
decodeErrorParser _ (Right x) = pure x