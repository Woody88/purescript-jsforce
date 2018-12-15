module Salesforce.WebSpec where 

import Prelude
import Salesforce.Connection.Web.Types
import Test.Utils

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Argonaut.Core as J
import Foreign as Foreign
import Foreign.Object as Foreign
import Salesforce.Connection (ConnectionAuth(..), ConnectionError)
import Salesforce.Connection.Web.Types (OauthResponse(..))
import Salesforce.Connection.Web (parseSFOauthResponse, setOauthResponseDecodeTags, decodeResponse)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldContain, shouldEqual)



spec :: Spec Unit
spec =
  describe "can decode OauthResponse to JSON" do
    testDecodeJSON
        """{"tag": "OauthConnection", "contents": { "access_token": "sometoken", "token_type": "Bearer", "refresh_token": "sometoken", "instance_url": "someurl", "id": "someid", "issued_at": "somedate", "signature": "somesig"} }"""
        (Right (OauthConnection expectedConnAuth))

    it "can retrieve url query params" do
        parseSFOauthResponse locationState `shouldEqual` (Right (RouteQuery routeQueryParams)) 

    it "sets OauthConnectionError tag property in json object" do
        case  parseSFOauthResponse locationErrorState of
            (Right rq) -> do
                let (RouteQuery rq') = setOauthResponseDecodeTags rq 
                decodeResponse rq' `shouldEqual` (Right (OauthConnectionError { error: "erroTest", error_description: (Just "test"), state: Nothing }))  

            _ -> true `shouldEqual` false
    where 
        expectedConnAuth = 
            { access_token  : "sometoken"
            , token_type    : Just "Bearer"
            , refresh_token : Just "sometoken" 
            , scope         : Nothing
            , state         : Nothing 
            , instance_url  : "someurl"
            , id            : "someid"
            , issued_at     : "somedate" 
            , signature     : "somesig" 
            }

        locationState = 
            { state: Foreign.unsafeToForeign {}
            , path: "/"
            , pathname: "https://example.com#field1=test&field2=test"
            , hash: "#field1=test&field2=test"
            , search: ""
            }

        locationErrorState = 
            { state: Foreign.unsafeToForeign {}
            , path: "/"
            , pathname: "https://example.com#error=test&error_description=test"
            , hash: "#error=erroTest&error_description=test"
            , search: ""
            }
        

        routeQueryParams =
            Foreign.singleton "field1" "test" <> Foreign.singleton "field2" "test"
