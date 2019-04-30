module Salesforce.Types where

import Prelude

import Control.Monad.Except.Trans (class MonadThrow, ExceptT, runExceptT)
import Control.Monad.Reader.Trans (class MonadAsk, ReaderT, ask, runReaderT)
import Data.Either (Either)
import Data.Maybe (Maybe)
import Data.Variant (Variant)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Salesforce.Connection.Types (Connection)

newtype SalesforceM e a = SalesforceM (ReaderT Connection (ExceptT e Aff) a)
type Salesforce e a = SalesforceM e a 
type SalesforceV e a = SalesforceM (Variant e) a 

derive newtype instance functorSalesforceM       :: Functor (SalesforceM e)
derive newtype instance applySalesforceM         :: Apply (SalesforceM e)
derive newtype instance applicativeSalesforceM   :: Applicative (SalesforceM e)
derive newtype instance bindSalesforceM          :: Bind (SalesforceM e)
derive newtype instance monadSalesforceM         :: Monad (SalesforceM e)


derive newtype instance monadEffectSalesforceM   :: MonadEffect (SalesforceM e)
derive newtype instance monadAffectSalesforceM   :: MonadAff (SalesforceM e)

derive newtype instance monadAskSalesforceM      :: MonadAsk Connection (SalesforceM e)
derive newtype instance monadThrowSalesforceM    :: MonadThrow e (SalesforceM e)

runSalesforce :: forall e a. SalesforceM e a -> Connection -> Aff (Either e a)
runSalesforce (SalesforceM f) conn = runExceptT $ runReaderT f conn

salesforce :: forall e a. ReaderT Connection (ExceptT e Aff) a -> Salesforce e a
salesforce = SalesforceM

getConnection :: forall e. SalesforceM e Connection 
getConnection = ask

foreign import kind NetworkType  

foreign import data Affjax :: NetworkType 

data NTProxy (n :: NetworkType) = NTProxy 

type NetworkError = { errorCode :: String, message :: String, fields :: Maybe (Array String)}


