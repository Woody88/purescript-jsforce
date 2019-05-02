module Test.TestM where 

import Prelude

import Control.Monad.Except.Trans (ExceptT(..), runExceptT)
import Control.Monad.Reader.Trans (class MonadAsk, class MonadReader, ReaderT(..), ask, runReaderT)
import Control.Plus (empty)
import Data.Argonaut.Core (fromString)
import Data.Argonaut.Parser (jsonParser)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe)
import Data.Variant (Variant)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Salesforce.Connection.Types (Connection)
import Salesforce.Internal (class HasNetwork, class HasNetworkType)
import Salesforce.Query.Types (QueryEndpoint(..), SOQL(..), totalSize)
import Salesforce.Types (NetworkError, kind NetworkType)
import Salesforce.Util (EitherV)
import Unsafe.Coerce (unsafeCoerce)

newtype TestM a = TestM (ReaderT Connection Aff a)
type Test e a = ExceptT e TestM a 
type TestV e a = Test (Variant e) a 

foreign import data TestNetwork :: NetworkType

derive newtype instance functorTestM       :: Functor TestM
derive newtype instance applyTestM         :: Apply TestM
derive newtype instance applicativeTestM   :: Applicative TestM
derive newtype instance bindTestM          :: Bind TestM
derive newtype instance monadTestM         :: Monad TestM

derive newtype instance monadEffectTestM   :: MonadEffect TestM
derive newtype instance monadAffectTestM   :: MonadAff TestM

derive newtype instance monadReaderTestM   :: MonadReader Connection TestM
derive newtype instance monadAskTestM      :: MonadAsk Connection TestM

runTest :: forall a. TestM a -> Connection -> Aff a
runTest (TestM f) conn = runReaderT f conn

runTestT :: forall e a. Test e a -> Connection -> Aff (Either e a)
runTestT s conn = flip runTest conn $ runExceptT s 

runTestV :: forall e a. TestV e a -> Connection -> Aff (EitherV e a)
runTestV = runTestT 

test :: forall e a. ReaderT Connection Aff (Either e a) -> Test e a
test = ExceptT <<< TestM

testV :: forall e a. ReaderT Connection Aff (EitherV e a) -> TestV e a
testV = test

instance hasTestNetworkType :: HasNetworkType TestM TestNetwork

instance hasTestQueryNetwork :: HasNetwork TestM (QueryEndpoint r) TestNetwork where 
    request endpoint = TestM $ ReaderT \conn -> do 
        case endpoint of 
            Query soql -> queryCase soql 
            otherwise -> pure $ Left $ { errorCode: "Not implemented", message: "Not implemented", fields: empty}


queryCase = case _ of 
    SOQL "Select Name from Account" -> 
        pure $ Right $ unsafeCoerce { done: true, totalSize: 1, records: [{name: "Account 1"}] }

    otherwise -> 
        pure $ Left $ { errorCode: "Bad Query", message: "Bad Query", fields: empty}