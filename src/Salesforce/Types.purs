module Salesforce.Types where

import Control.Monad.Error.Class
import Control.Monad.Except.Trans
import Data.Either
import PSObject.Standard
import Prelude

import Control.Monad.Reader.Class (class MonadAsk)
import Control.Monad.Reader.Trans (ReaderT(..), runReaderT, ask)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect)
import Effect.Class (class MonadEffect, liftEffect)
import Foreign (MultipleErrors)
import Foreign.Class (class Decode, class Encode, decode, encode)
import Foreign.Generic (genericDecode, genericEncode, decodeJSON, genericEncodeJSON, encodeJSON, defaultOptions)
-- import Unsafe.Coerce (unsafeCoerce)

foreign import data Connection :: Type
foreign import data Client     :: Type
foreign import data PSForce    :: Type

type RecordResult = { id :: String
                    , success :: Boolean
                    , errors :: Array String
                    }

data SFId = SFId String SObjectName

newtype SFData a = SFData a 

newtype SObjectName = SObjectName String 

newtype Username = Username String 
data Password = Password String String

sfdcId :: String -> SObjectName -> SFId
sfdcId = SFId

sobjectName  :: String -> SObjectName
sobjectName = SObjectName

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

derive instance genericSFData :: Generic (SFData a) _  

instance decodeAccount :: Decode a => Decode (SFData a)  where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}

instance encodeAccount :: Encode a => Encode (SFData a) where
  encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}

type Account = { name :: String }

instance showAccount :: Show a => Show (SFData a) where 
  show = genericShow 

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

instance monadSalesforceM :: Monad SalesforceM  

instance monadEffectSalesforceM :: MonadEffect SalesforceM where
  liftEffect a = SalesforceM \_ -> liftEffect a

instance monadAffectSalesforceM :: MonadAff SalesforceM where
  liftAff a = SalesforceM \_ -> a

runSalesforceBrowser :: forall a. SalesforceBrowserM a -> Client -> Aff a
runSalesforceBrowser (SalesforceBrowserM f) client =
  f client

runSalesforce :: forall a. SalesforceM a -> Connection -> Aff a
runSalesforce (SalesforceM f) conn =
  f conn

salesforce :: forall e a. (Connection -> Aff (Either e a)) -> Salesforce e a
salesforce = ExceptT <<< SalesforceM

-- salesforceT :: forall e a. (Connection -> ExceptT e Aff a) -> Salesforce e a
-- salesforceT = ExceptT <<< SalesforceM <<< runExceptT

runSalesforceT :: forall e a. Salesforce e a -> Connection -> Aff (Either e a)
runSalesforceT s conn = do
  let (SalesforceM f) = runExceptT s
  f conn

-- undefined :: forall a. a 
-- undefined = unsafeCoerce unit

decodeErrorParser :: forall a error. (MultipleErrors -> error) -> Either MultipleErrors a -> Either error a
decodeErrorParser error (Left x) = throwError $ error x
decodeErrorParser _ (Right x) = pure x
