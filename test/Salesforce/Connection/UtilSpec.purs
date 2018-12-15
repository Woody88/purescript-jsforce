module Salesforce.Connection.UtilSpec where 

import Prelude
import Data.Either (Either(..))
import Salesforce.Connection (RequestError(..), parseUserInfoFromUrl, idUrlRegex)
import Salesforce.Types.Common (UserInfo(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)


spec :: Spec Unit
spec =
    describe "Test Connection Utils" do
        it "can retrieve user id from url with regex" do
            (parseUserInfoFromUrl DecodeError idUrlRegex idUrl) `shouldEqual` (Right (UserInfo {userId: "00510000006wLBfAAM", orgId: "00Dp00000004kzKEAQ", url: "https://test.salesforce.com/id/00Dp00000004kzKEAQ/00510000006wLBfAAM"}))

    where 
        idUrl = "https://test.salesforce.com/id/00Dp00000004kzKEAQ/00510000006wLBfAAM"
        