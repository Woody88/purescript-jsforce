module Salesforce.Types where

import Prelude

import Control.Monad.Except.Trans (ExceptT(..), runExceptT)
import Control.Monad.Reader.Trans (class MonadAsk, class MonadReader, ReaderT, ask, runReaderT)
import Data.Either (Either)
import Data.Maybe (Maybe)
import Data.Variant (Variant)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Salesforce.Connection.Types (Connection)
import Salesforce.Util (EitherV)

newtype SalesforceM a = SalesforceM (ReaderT Connection Aff a)
type Salesforce e a = ExceptT e SalesforceM a 
type SalesforceV e a = Salesforce (Variant e) a 

derive newtype instance functorSalesforceM       :: Functor SalesforceM
derive newtype instance applySalesforceM         :: Apply SalesforceM
derive newtype instance applicativeSalesforceM   :: Applicative SalesforceM
derive newtype instance bindSalesforceM          :: Bind SalesforceM
derive newtype instance monadSalesforceM         :: Monad SalesforceM


derive newtype instance monadEffectSalesforceM   :: MonadEffect SalesforceM
derive newtype instance monadAffectSalesforceM   :: MonadAff SalesforceM


derive newtype instance monadReaderSalesforceM   :: MonadReader Connection SalesforceM
derive newtype instance monadAskSalesforceM      :: MonadAsk Connection SalesforceM


runSalesforce :: forall a. SalesforceM a -> Connection -> Aff a
runSalesforce (SalesforceM f) conn = runReaderT f conn

runSalesforceT :: forall e a. Salesforce e a -> Connection -> Aff (Either e a)
runSalesforceT s conn = flip runSalesforce conn $ runExceptT s 

runSalesforceV :: forall e a. SalesforceV e a -> Connection -> Aff (EitherV e a)
runSalesforceV = runSalesforceT 

salesforce :: forall e a. ReaderT Connection Aff (Either e a) -> Salesforce e a
salesforce = ExceptT <<< SalesforceM

salesforceV :: forall e a. ReaderT Connection Aff (Either (Variant e) a) -> SalesforceV e a
salesforceV = salesforce

getConnection :: SalesforceM Connection 
getConnection = ask

foreign import kind NetworkType  

foreign import data Affjax :: NetworkType 

data NTProxy (n :: NetworkType) = NTProxy 

type NetworkError = { errorCode :: String, message :: String, fields :: Maybe (Array String)}


