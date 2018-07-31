module Salesforce.Types where

import Prelude
import Data.Either
import Control.Monad.Except.Trans
import PSObject.Standard

import Effect.Aff (Aff)

foreign import data Connection :: Type
foreign import data Client     :: Type
foreign import data PSForce    :: Type


newtype Username = Username String 
newtype Password = Password String 

newtype SalesforceM a = SalesforceM (Connection -> Aff a)
type Salesforce e a = ExceptT e SalesforceM a

newtype SalesforceBrowserM a = SalesforceBrowserM (Client -> Aff a)
type SalesforceBrowser = SalesforceBrowserM Unit

-- lift :: forall a. SalesforceM a -> SalesforceBrowserM a
-- lift (SalesforceM f) = do
--   conn <- getConnection
--   pure $ f conn
