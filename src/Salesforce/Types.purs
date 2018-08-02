module Salesforce.Types where

import Prelude
import Data.Either
import Control.Monad.Except.Trans
import PSObject.Standard
import Control.Monad.Reader.Class (class MonadAsk)
import Control.Monad.Reader.Trans (ReaderT(..), runReaderT, ask)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
-- import Unsafe.Coerce (unsafeCoerce)

foreign import data Connection :: Type
foreign import data Client     :: Type
foreign import data PSForce    :: Type


newtype Username = Username String 
data Password = Password String String

newtype SalesforceT a = SalesforceT (ReaderT Connection Aff a)
newtype SalesforceM a = SalesforceM (Connection -> Aff a)
type Salesforce e a = ExceptT e SalesforceM a

newtype SalesforceBrowserM a = SalesforceBrowserM (Client -> Aff a)

type SalesforceBrowser = SalesforceBrowserM Unit

newtype SOQLM a = SOQLM (Connection -> Effect a)
-- lift :: forall a. SalesforceM a -> SalesforceBrowserM a
-- lift (SalesforceM f) = do
--   conn <- getConnection
--   pure $ f conn


instance functorSalesforceM :: Functor SalesforceM where
  map f s = SalesforceM \c -> runSalesforce s c >>= f >>> pure 


instance applySalesforceM:: Apply SalesforceM where
  apply (SalesforceM f) s = SalesforceM \c -> f c <*> runSalesforce s c 


instance applicativeSalesforceM :: Applicative SalesforceM where 
  pure x = SalesforceM \c -> pure x

instance bindSalesforceM :: Bind SalesforceM where
  bind (SalesforceM s) f = SalesforceM \c -> do 
    a <- s c
    let (SalesforceM g) = f a
    g c

runSalesforceBrowser :: forall a. SalesforceBrowserM a -> Client -> Aff a
runSalesforceBrowser (SalesforceBrowserM f) client =
  f client

runSalesforce :: forall a. SalesforceM a -> Connection -> Aff a
runSalesforce (SalesforceM f) conn =
  f conn

-- undefined :: forall a. a 
-- undefined = unsafeCoerce unit