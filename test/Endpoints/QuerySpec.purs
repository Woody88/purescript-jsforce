module Test.Endpoints.QuerySpec where 


import Prelude
import Type.Row

import Control.Monad.Error.Class (throwError)
import Control.Plus (empty)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, getField)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Either (Either(..), either, isLeft)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Variant (SProxy(..), default, match, on)
import Effect.Aff (Aff)
import Salesforce.Connection.Types (Connection(..))
import Salesforce.Query.Internal (query, queryParseError)
import Salesforce.Query.Types (QueryError, QueryResult(..), SOQL(..), records, totalSize)
import Salesforce.Util (EitherV)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual, shouldSatisfy)
import Test.TestM (TestM(..), TestV, runTest)
import Unsafe.Coerce (unsafeCoerce)

spec :: Spec Unit
spec =   
    describe "Query Endpoint" do
        it "returns a query result on SOQL Account query" do
            result <- runTest queryAccount fakeConnection 
            case result of 
                Left v -> flip match v 
                            { queryError: \n -> fail n.message
                            , queryParseError: \s -> fail s 
                            }
                Right res -> do 
                    totalSize res `shouldEqual` 1
                    case records res of 
                        [account] -> account `shouldEqual` (Account $ { name: "Account 1"})
                        otherwise -> fail "Should only have one record"

        it "returns a Query Error on bad SOQL query" do 
            let queryErr = default (fail "Did not get correct error output")
                            # on _queryError (\qe -> qe.message `shouldEqual` "Bad Query")
                            
            result <- runTest queryBadAccount fakeConnection 
            either queryErr (\_ -> fail "Should not get a query result") result

fakeConnection :: Connection 
fakeConnection = unsafeCoerce ""

queryAccount :: forall r. TestM (EitherV (QueryError + r) (QueryResult Account))
queryAccount = do 
    query $ SOQL "Select Name from Account"

queryBadAccount :: forall r. TestM (EitherV (QueryError + r) (QueryResult Account))
queryBadAccount = do 
    query $ SOQL "Select Name from BadAccount"

_queryError = SProxy :: SProxy "queryError"

newtype Account 
    = Account { name     :: String
              } 
derive instance genericAccount :: Generic Account _ 
derive instance eqAccount :: Eq Account 
instance genericShowAccount :: Show Account where 
    show = genericShow

instance decodeJsonAccount :: DecodeJson Account where 
    decodeJson json = do 
        obj <- decodeJson json
        name <- getField obj "name"
        pure $ Account { name }