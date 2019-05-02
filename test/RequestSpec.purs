module Test.RequestSpec where 

import Prelude

import Affjax (Response, ResponseFormatError(..))
import Affjax.StatusCode (StatusCode(..))
import Control.Plus (empty)
import Data.Argonaut.Core (Json)
import Data.Either (Either(..))
import Foreign (ForeignError(..), unsafeToForeign)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

-- spec :: Spec Unit
-- spec =   
--     describe "Request Function" do
--         it "it returns a Network Parse Error on response format error" do
--             case validateRequest dummyResponse of 
--                 Left nterr -> nterr `shouldEqual` expectedNterr
--                 Right _ -> fail "Should have gotten a Network Parse Error"

-- expectedNterr :: NetworkError
-- expectedNterr = { errorCode: "Network Parse Error", message: "Server Error", fields: empty}

-- dummyResponse :: Response (Either ResponseFormatError Json) 
-- dummyResponse = 
--     { status: StatusCode 500, statusText: "Server Error", headers: mempty, body: Left $ responseFormatError }
--     where 
--         responseFormatError = ResponseFormatError (ForeignError "Server Error") (unsafeToForeign {})