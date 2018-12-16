module Salesforce.Connection.Web where


import Data.List
import Data.Tuple
import Prelude

import Control.Monad.Except (runExcept)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as J
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.Foldable (oneOf, foldl)
import Data.Map as Map
import Data.Maybe (Maybe, maybe)
import Data.String as String
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Foreign (Foreign)
import Foreign.Class (decode, encode)
import Foreign.JSON (decodeJSONWith)
import Foreign.Object (Object)
import Foreign.Object as Foreign
import Record as Record
import Routing (match)
import Routing.Match (Match, lit, params)
import Routing.PushState (LocationState)
import Routing.Types as Routing
import Salesforce.Connection (Connection(..), ConnectionError(..), ConnectionAuth(..), CommonConfig, ClientId(..), parseUserInfoFromUrl, idUrlRegex)
import Salesforce.Connection.Web.Types (OauthError(..), OauthResponse(..), RouteQuery(..))
import Web.Event.CustomEvent (CustomEvent, toEvent)
import Web.Event.Event (Event, EventType(..))
import Web.Event.EventTarget (EventListener, addEventListener, dispatchEvent, eventListener)
import Web.HTML.Window (Window)
import Web.HTML.Window as Window

foreign import windowOpener :: Effect Window 

foreign import windowClose :: Window -> Effect Unit 

foreign import sforceConnectedImpl :: Event -> Foreign

foreign import sforceConnectedEventImpl :: EventType -> Foreign -> CustomEvent

sforceConnected :: Event -> Either OauthError Connection
sforceConnected e =  (runExcept <<< decode $ sforceConnectedImpl e) # handleDecodeError

sforceConnectedEvent :: EventType -> Either OauthError Connection -> CustomEvent 
sforceConnectedEvent et eitherConnection = sforceConnectedEventImpl et $ either encode encode eitherConnection

loginOauth :: { serverUrl :: String, response_type :: String, clientId :: ClientId, redirect_uri :: String, scope :: String } -> Window -> (Either OauthError Connection -> Effect Unit) -> Effect (Maybe Window)
loginOauth {serverUrl, response_type, clientId: (ClientId clientid), redirect_uri, scope} window f = do
    let url = serverUrl <> "?response_type=" <> response_type <> "&client_id=" <> clientid <> "&redirect_uri=" <> redirect_uri <> "&scope=" <> scope
    
     -- Open Sforce Login Window
    authWindow <- Window.open url "SForce Login Page" "_blank" window

    -- event listener
    sfLoginListener <- eventListener (\e -> (f $ sforceConnected e) *> (maybe (pure unit) windowClose authWindow))

    -- Set connection listen to main window
    addEventListener (EventType "sforceConnected") sfLoginListener false $ Window.toEventTarget window
    
    log "Added Event Listener"
    
    pure authWindow

-- -- Should the previous sforceConnected listener be removed when this function is called?
connectOauth :: LocationState -> Window -> Effect Unit 
connectOauth location window = do
    let eitherRouteQuery = ((setOauthResponseDecodeTags <$> parseSFOauthResponse location) # (lmap DecodeError)) :: Either OauthError (RouteQuery Json)
        eitherResponse = eitherRouteQuery >>= \(RouteQuery json) -> decodeResponse json
        eitherConnection = eitherResponse >>= mkConnection 

    void $ dispatchEvent (connectionEvent eitherConnection) (Window.toEventTarget window)

    where 
      mkConnection :: OauthResponse -> Either OauthError Connection 
      mkConnection oauthResp = case oauthResp of
        (OauthConnectionError connError) -> 
          Left <<< Error $ connError.error

        (OauthConnection connAuth) -> do
          userInfo <- parseUserInfoFromUrl DecodeError idUrlRegex connAuth.id
          pure <<< Connection $ Record.merge  {userInfo} connAuth
          
      connectionEvent :: Either OauthError Connection -> Event 
      connectionEvent eitherConnection = toEvent $ sforceConnectedEvent (EventType "sforceConnected") eitherConnection


decodeResponse :: Json -> Either OauthError OauthResponse
decodeResponse json =  ((runExcept $ runDecoder $ J.stringify json) # handleDecodeError)

-- Adds a key with name as 'tag' for decoding purpose
setOauthResponseDecodeTags :: RouteQuery (Object String) -> RouteQuery Json
setOauthResponseDecodeTags (RouteQuery rq) = do
  let hasError  = Foreign.member "error" rq 
      hasAccess = Foreign.member "access_token" rq 
  RouteQuery <<< decodeResponse rq $ (Tuple hasError hasAccess)

  where 
    decodeResponse :: forall a. Object String -> Tuple Boolean Boolean -> Json
    decodeResponse jsonObj (Tuple error access) = case error, access of
      true, false ->  encodeJson $ Foreign.insert "tag" (J.fromString "OauthConnectionError") $ contentsObject jsonObj 
      false, true ->  encodeJson $ Foreign.insert "tag" (J.fromString "OauthConnection") $ contentsObject jsonObj
      _, _        ->  encodeJson $ Foreign.insert "tag" (J.fromString "OauthError") $ contentsObject failingObj

      where 
        contentsObject = Foreign.singleton "contents" <<< encodeJson
        
    failingObj :: Object String 
    failingObj = Foreign.singleton "error" "fail to decode response could not find error or access property"

parseSFOauthResponse :: LocationState -> Either String (RouteQuery (Object String)) 
parseSFOauthResponse location = (RouteQuery <<< toJsonObject <<< mapToList) <$> (match queryParams $ (concatleadingSlash <<< replaceHashToQuery) location.hash) 
    where 
      concatleadingSlash = (<>) "/"  
        
      replaceHashToQuery = String.replace (String.Pattern "#") (String.Replacement "?")

      toJsonObject = foldl (\b (Tuple a c) -> Foreign.insert a c b) Foreign.empty 

      mapToList :: Map.Map String String -> List (Tuple String String)
      mapToList = Map.toUnfoldable

queryParams :: Match (Map.Map String String)
queryParams = (lit "" *> params)

runDecoder = decodeJSONWith decode

handleDecodeError = lmap (\err -> DecodeError $ show err)


