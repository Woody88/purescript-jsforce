module UrlTest where
  
import Prelude
import Prim.Row

import Affjax.RequestBody (RequestBody(..))
import Effect (Effect)
import Record as R
import URI.AbsoluteURI (Authority(..), HierPath, HierarchicalPart(..), Host(..), Path(..), PathAbsolute(..), PathRootless(..), Port, Query, AbsoluteURI(..), AbsoluteURIOptions, UserInfo)
import URI.AbsoluteURI as AbsoluteURI
import URI.Host.RegName as RegName
import URI.HostPortPair (HostPortPair)
import URI.HostPortPair as HostPortPair
import URI.Path.Segment as PathSegment
import URI.Port as Port
import URI.Query as Query
import URI.Scheme as Scheme
import Unsafe.Coerce (unsafeCoerce)


options ∷ Record (AbsoluteURIOptions UserInfo (HostPortPair Host Port) Path HierPath Query)
options =
  { parseUserInfo: pure
  , printUserInfo: identity
  , parseHosts: HostPortPair.parser pure pure
  , printHosts: HostPortPair.print identity identity
  , parsePath: pure
  , printPath: identity
  , parseHierPath: pure
  , printHierPath: identity
  , parseQuery: pure
  , printQuery: identity
}


type Config =
  ( username  :: String
  , password  :: String 
  , clientId  :: String 
  , connType  :: String 
  , sessionId :: String
  )

defaultConfig :: Record Config 
defaultConfig = 
    { username: mempty
    , password: mempty 
    , clientId: mempty 
    , connType: mempty 
    , sessionId: mempty
    }

login :: forall config. Union config _ Config => { | config } -> Record Config
login rec = login' rec defaultConfig

login' :: forall config a. Union config a Config => { | config } -> Record a -> Record Config 
login' r d = R.merge r d

-- xml :: String 
-- xml = 
  -- "<?xml version=\"1.0\" encoding=\"UTF-8\"?><soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envel ope/\" xmlns=\"urn:partner.soap.sforce.com\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"><soapenv:Body><login Response><result><metadataServerUrl>https://cs6.salesforce.com/services/Soap/m/42.0/00DN0000000B7Yh</metadataServerUrl><p asswordExpired>false</passwordExpired><sandbox>true</sandbox><serverUrl>https://cs6.salesforce.com/services/Soap/u/42.0/0 0DN0000000B7Yh</serverUrl><sessionId>00DN0000000B7Yh!ARsAQA_2VMwSyaxbcjWhcivH8NnSDhBweDejdZELJAHoQgWH4Yw4ekNcBlEw9K1tchgn HNFyNAhW4n5a0i_KVELUbzBcFq2q</sessionId><userId>00510000006wLBfAAM</userId><userInfo><accessibilityMode>false</accessibil ityMode><chatterExternal>false</chatterExternal><currencySymbol>￥</currencySymbol><orgAttachmentFileSizeLimit>20971520</ orgAttachmentFileSizeLimit><orgDefaultCurrencyIsoCode>JPY</orgDefaultCurrencyIsoCode><orgDefaultCurrencyLocale>ja_JP</org DefaultCurrencyLocale><orgDisallowHtmlAttachments>false</orgDisallowHtmlAttachments><orgHasPersonAccounts>false</orgHasPe rsonAccounts><organizationId>00DN0000000B7YhMAK</organizationId><organizationMultiCurrency>false</organizationMultiCurren cy><organizationName>Coca-Cola East Japan</organizationName><profileId>00e10000000n8HxAAI</profileId><roleId xsi:nil=\"true\"/><sessionSecondsValid>43200</sessionSecondsValid><userDefaultCurrencyIsoCode xsi:nil=\"true\"/><userEmail>sfdc_support_l3@avaxiaconsulting.com</userEmail><userFullName>AVAXIA SFDC3</userFullName><userId>00510000006wLBfAAM</userId><userLanguage>en_US</userLanguage><userLocale>ja_JP</userLocale><userName>ccejex-sfdc1224@ccej.co.jp.fulldev</userName><userTimeZone>Asia/Tokyo</userTimeZone><userType>Standard</userType><userUiSkin>Theme3</userUiSkin></userInfo></result></loginResponse></soapenv:Body></soapenv:Envelope>"  "<?xml version=\"1.0\" encoding=\"UTF-8\"?><soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envel ope/\" xmlns=\"urn:partner.soap.sforce.com\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"><soapenv:Body><login Response><result><metadataServerUrl>https://cs6.salesforce.com/services/Soap/m/42.0/00DN0000000B7Yh</metadataServerUrl><p asswordExpired>false</passwordExpired><sandbox>true</sandbox><serverUrl>https://cs6.salesforce.com/services/Soap/u/42.0/0 0DN0000000B7Yh</serverUrl><sessionId>00DN0000000B7Yh!ARsAQA_2VMwSyaxbcjWhcivH8NnSDhBweDejdZELJAHoQgWH4Yw4ekNcBlEw9K1tchgn HNFyNAhW4n5a0i_KVELUbzBcFq2q</sessionId><userId>00510000006wLBfAAM</userId><userInfo><accessibilityMode>false</accessibil ityMode><chatterExternal>false</chatterExternal><currencySymbol>￥</currencySymbol><orgAttachmentFileSizeLimit>20971520</ orgAttachmentFileSizeLimit><orgDefaultCurrencyIsoCode>JPY</orgDefaultCurrencyIsoCode><orgDefaultCurrencyLocale>ja_JP</org DefaultCurrencyLocale><orgDisallowHtmlAttachments>false</orgDisallowHtmlAttachments><orgHasPersonAccounts>false</orgHasPe rsonAccounts><organizationId>00DN0000000B7YhMAK</organizationId><organizationMultiCurrency>false</organizationMultiCurren cy><organizationName>Coca-Cola East Japan</organizationName><profileId>00e10000000n8HxAAI</profileId><roleId xsi:nil=\"true\"/><sessionSecondsValid>43200</sessionSecondsValid><userDefaultCurrencyIsoCode xsi:nil=\"true\"/><userEmail>sfdc_support_l3@avaxiaconsulting.com</userEmail><userFullName>AVAXIA SFDC3</userFullName><userId>00510000006wLBfAAM</userId><userLanguage>en_US</userLanguage><userLocale>ja_JP</userLocale><userName>ccejex-sfdc1224@ccej.co.jp.fulldev</userName><userTimeZone>Asia/Tokyo</userTimeZone><userType>Standard</userType><userUiSkin>Theme3</userUiSkin></userInfo></result></loginResponse></soapenv:Body></soapenv:Envelope>"