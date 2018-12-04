module Salesforce.Connection.Internal where

import Prelude

import Affjax as AX
import Affjax.RequestBody (formURLEncoded, string)
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.Except (runExcept, throwError)
import Data.Argonaut.Core as J
import Data.Array (foldl, foldr, take)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.FormURLEncoded (fromArray)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..))
import Data.String.Common (joinWith, split)
import Data.String.Regex (match)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Effect.Aff (Aff)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (logShow)
import Foreign.Class (decode)
import Foreign.JSON (decodeJSONWith)
import Record as Record
import Salesforce.Connection.Types (ClientId, ClientSecret, CommonConfig, Connection(..), ConnectionConfig(..), EnvironmentType(..), GrantType(..), Password(..), RequestError(..), SecretToken(..), SessionId(..), Username(..), ConnectionAuth(..), toFormUrlParam)
import Salesforce.Connection.Util (maybeToEither, getXmlElVal, getUserInfo)
import Salesforce.Types.Common (UserInfo(..))
import Web.HTML.Window (Window)
import Web.HTML.Window as Window

productionUrl :: String 
productionUrl = "https://login.salesforce.com"

sandboxUrl :: String 
sandboxUrl = "https://test.salesforce.com/"

soapBaseUrl :: String 
soapBaseUrl = "services/Soap/u"

tokenBaseUrl :: String 
tokenBaseUrl = "services/oauth2/token"

soapUserPassBody :: Username -> Password -> String 
soapUserPassBody (Username u) (Password p (SecretToken t)) = 
    foldl append mempty $ 
        ["<se:Envelope xmlns:se=\"http://schemas.xmlsoap.org/soap/envelope/\">",
        "<se:Header/>",
        "<se:Body>",
            "<login xmlns=\"urn:partner.soap.sforce.com\">",
            "<username>" <> u <> "</username>",
            "<password>" <> p <> t <> "</password>",
            "</login>",
        "</se:Body>",
        "</se:Envelope>"
        ]

login :: ConnectionConfig -> Aff (Either RequestError Connection)
login (Soap configs)          = fromUserPasswordSoap configs
login (UserPassOauth configs) = fromUserPasswordOauth configs
login _ = pure <<< Left $ ResponseDecodeError "Error"

fromUserPasswordSoap :: { | CommonConfig () } -> Aff (Either RequestError Connection)  
fromUserPasswordSoap {username, password, envType, version } = 
    fromUserPasswordSoap' username password  envType version

fromUserPasswordSoap' :: Username -> Password -> EnvironmentType -> Number -> Aff (Either RequestError Connection)
fromUserPasswordSoap' user pswd env version = do 
    res  <- AX.request (AX.defaultRequest { url = url'
                                          , method = Left POST
                                          , responseFormat = ResponseFormat.string
                                          , content = Just body 
                                          , headers = [ RequestHeader "Access-Control-Allow-Origin" "*", RequestHeader "content-type" "text/xml", RequestHeader "SOAPAction" "\"\"" ]
                                          }) 
    pure $ do 
        xmlDoc <- res.body # handleSoapResponseError 
        handleSoapResponse envurl xmlDoc
    where 
        envurl = envUrl env
        url' = foldr (\b a -> b <> "/" <> a) mempty [envurl, soapBaseUrl, show version]
        body = string $ soapUserPassBody user pswd

handleSoapResponse :: String -> String -> Either RequestError Connection  
handleSoapResponse envUrl xml = do
        userId    <- (getXmlElVal $ match userIdReg xml )   `maybeToEither` parseXmlError "userId"
        orgId     <- (getXmlElVal $ match orgIdReg xml )    `maybeToEither` parseXmlError "orgId"
        serverUrl <- (getXmlElVal $ match serverUrlReg xml) `maybeToEither` parseXmlError "serverUrl"
        sessionId <- (getXmlElVal $ match sessionIdReg xml) `maybeToEither` parseXmlError "sessionId"
        pure $  Connection { access_token: sessionId
                           , token_type: Just "Bearer"
                           , instance_url: getHost serverUrl  --- need to fix this passing url with version number and what not
                           , id: serverUrl <> "/id/" <> userId 
                           , refresh_token: Nothing 
                           , scope: Nothing 
                           , state: Nothing 
                           , issued_at: mempty
                           , signature: mempty 
                           , userInfo: UserInfo { userId, orgId, url: serverUrl}
                           }
    where 
        getHost url  = joinWith "/" $ take 3 $ split (Pattern "/")  url
        serverUrlReg = unsafeRegex "<serverUrl>([^<]+)</serverUrl>" noFlags 
        sessionIdReg = unsafeRegex "<sessionId>([^<]+)</sessionId>" noFlags 
        userIdReg    = unsafeRegex "<userId>([^<]+)</userId>" noFlags 
        orgIdReg     = unsafeRegex "<organizationId>([^<]+)</organizationId>" noFlags 
        parseXmlError err = ResponseDecodeError $ "Could not parse xml element: " <> err

handleSoapResponseError = lmap (\err -> do
    let faultMatch = match faultstringReg $ AX.printResponseFormatError err
    ResponseDecodeError $ fromMaybe "Soap response Error" $ getXmlElVal faultMatch)
    where 
        faultstringReg = unsafeRegex "<faultstring>([^<]+)</faultstring>" noFlags 

fromSession :: { sessionId :: SessionId, serverUrl :: String | CommonConfig () } -> Aff (Either RequestError Connection) 
fromSession {sessionId: (SessionId sessionId), serverUrl, envType} = pure do 
    userInfo <- parseUserInfoFromIdUrl ResponseDecodeError serverUrl 
    pure do 
        Connection { access_token:  sessionId
                   , token_type:    Nothing
                   , refresh_token: Nothing 
                   , scope:         Nothing
                   , state:         Nothing 
                   , instance_url:  serverUrl
                   , id:            mempty
                   , issued_at:     mempty 
                   , signature:     mempty
                   , userInfo
                   }

fromSession' :: { sessionId :: SessionId, serverUrl :: String | CommonConfig () } -> Window ->  Effect (Maybe Window) 
fromSession'  {sessionId: (SessionId sessionId), serverUrl, envType} window = Window.open serverUrl "SForce Login Page" mempty window

fromUserPasswordOauth :: { clientId :: ClientId, clientSecret :: ClientSecret | CommonConfig () } -> Aff (Either RequestError Connection)  
fromUserPasswordOauth {clientId, clientSecret, username, password, envType } = 
    fromUserPasswordOauth' username password clientId clientSecret envType 

fromUserPasswordOauth' :: Username -> Password -> ClientId -> ClientSecret -> EnvironmentType -> Aff (Either RequestError Connection) 
fromUserPasswordOauth' user pswd cs cid env = do
    res  <- AX.post ResponseFormat.json url body 
    pure $ do 
        json     <- res.body # handleResponseError url
        (ConnectionAuth connAuth) <- ((runExcept $ runDecoder $ J.stringify json) # handleDecodeError) :: Either RequestError ConnectionAuth
        userInfo <- parseUserInfoFromIdUrl ResponseDecodeError connAuth.id 
        pure <<< Connection $ Record.merge {userInfo: userInfo} connAuth 

    where 
        grantType = GPassword 
        envurl = envUrl env
        url  = envurl <> tokenBaseUrl
        body = formURLEncoded <<< fromArray $ [toFormUrlParam user, toFormUrlParam pswd, toFormUrlParam cid, toFormUrlParam cs, toFormUrlParam grantType]

runDecoder = decodeJSONWith decode

handleResponseError url = lmap (\err -> ResponseDecodeError $ "POST " <> url <> " response failed to decode: " <> AX.printResponseFormatError err)

handleDecodeError = lmap (\err -> DecodeError $ show err)

envUrl :: EnvironmentType -> String 
envUrl Production = productionUrl
envUrl Sandbox    = sandboxUrl

{-
  This function function retrieves the organization Id and user Id from 
  the Id url returned from an authenticated connection.
-}
parseUserInfoFromIdUrl :: forall e. (String -> e) -> String -> Either e UserInfo 
parseUserInfoFromIdUrl f url = do 
    v <- (pure $ getUserInfo $ match rg url) `maybeToEither` parseIdUrlError
    case v of
        [(Just userId), (Just orgId)] -> pure $ UserInfo  { userId, orgId, url}
        _  -> throwError parseIdUrlError
    where
        rg = unsafeRegex "\\/(\\w+)\\/id\\/(\\w+)\\/" noFlags
        parseIdUrlError = f "Id url regex failed."
        