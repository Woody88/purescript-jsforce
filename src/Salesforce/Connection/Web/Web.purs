module Salesforce.Connection.Web where


import Data.List
import Data.Tuple
import Prelude

import Control.Monad.Except (runExcept)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as J
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (oneOf, foldl)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.String as String
import Effect (Effect)
import Foreign.Class (decode)
import Foreign.JSON (decodeJSONWith)
import Foreign.Object (Object)
import Foreign.Object as Foreign
import Record as Record
import Routing (match)
import Routing.Match (Match, lit, params)
import Routing.PushState (LocationState)
import Routing.Types (RoutePart)
import Routing.Types as Routing
import Salesforce.Connection (Connection(..), ConnectionError(..), ConnectionAuth(..), CommonConfig, ClientId(..), parseUserInfoFromIdUrl)
import Salesforce.Connection.Web.Types (OauthError(..), OauthResponse(..))
import Web.Event.CustomEvent (CustomEvent, toEvent)
import Web.Event.Event (Event, EventType(..))
import Web.Event.EventTarget (EventListener, addEventListener, dispatchEvent, eventListener)
import Web.HTML.Window (Window)
import Web.HTML.Window as Window

foreign import sforceConnected :: Event -> Either OauthError Connection

foreign import sforceConnectedEvent :: EventType -> Either OauthError Connection -> CustomEvent

newtype RouteQuery = RouteQuery (Map.Map String String) 

instance showRouteQuery :: Show RouteQuery where
  show (RouteQuery q) = show q


loginOauth :: { serverUrl :: String, response_type :: String, clientId :: ClientId, redirect_uri :: String, scope :: String } -> Window -> (Either OauthError Connection -> Effect Unit) -> Effect (Maybe Window)
loginOauth {serverUrl, response_type, clientId: (ClientId clientid), redirect_uri, scope} window f = do

    -- event listener
    sfLoginListener <- eventListener (\e -> f $ sforceConnected e)

    -- Set connection listen to main window
    addEventListener (EventType "sforceConnected") sfLoginListener false $ Window.toEventTarget window

    let url = serverUrl <> "?response_type=" <> response_type <> "&client_id=" <> clientid <> "&redirect_uri=" <> redirect_uri <> "&scope=" <> scope
    -- Open Sforce Login Window
    Window.open url "SForce Login Page" "_blank" window


-- Should the previous sforceConnected listener be removed when this function is called?
connectOauth :: LocationState -> Window -> Effect Unit 
connectOauth location window = do
    let eitherQuery = ((setDecodeTag <$> parseSFOauthResponse location) # (lmap DecodeError)) :: Either OauthError RouteQuery
    
        eitherConnection = eitherQuery >>= (decodeResponse <<< encodeJson <<< parseToForeignObject)  
      
    void $ dispatchEvent (connectionEvent eitherConnection) (Window.toEventTarget window) 

    where
      connectionEvent :: Either OauthError Connection -> Event 
      connectionEvent eitherConnection = toEvent $ sforceConnectedEvent (EventType "sforceConnected") eitherConnection

      parseToForeignObject :: RouteQuery -> Object String
      parseToForeignObject (RouteQuery m) = foldl f Foreign.empty $ (Map.toUnfoldable m :: List (Tuple String String )) 
        where 
          f jsonObj (Tuple k v) = Foreign.insert k v jsonObj

decodeResponse :: Json -> Either OauthError Connection
decodeResponse json = do 
  let decodeResponse' = ((runExcept $ runDecoder $ J.stringify json) # handleDecodeError) :: Either OauthError OauthResponse
  transformError decodeResponse'
  
  where 
    transformError :: Either OauthError OauthResponse -> Either OauthError Connection
    transformError r = case r of
      (Right (OauthConnection (ConnectionAuth connAuth))) -> do
        userInfo <- parseUserInfoFromIdUrl DecodeError connAuth.id 
        pure <<< Connection $ Record.merge  {userInfo} connAuth

      (Right (OauthConnectionError (ConnectionError connError))) -> 
        Left <<< Error $ connError.error

      (Left decodeError) -> 
        Left <<< DecodeError $ show decodeError

-- Adds a key with name as 'tag' for decoding purpose
setDecodeTag :: RouteQuery -> RouteQuery 
setDecodeTag (RouteQuery m) = RouteQuery <<< foldl f m $ toList m
  where 
    toList :: Map.Map String String -> List (Tuple String String)
    toList = Map.toUnfoldable 

    f :: Map.Map String String -> Tuple String String -> Map.Map String String 
    f m' (Tuple k v) = case k of
      "error"        -> Map.insert "tag" "OauthError" m'
      "access_token" -> Map.insert "tag" "OauthConnection" m' 
      _              -> m

parseSFOauthResponse :: LocationState -> Either String RouteQuery 
parseSFOauthResponse location = match queryParams $ (concatleadingSlash <<< replaceHashToQuery) location.hash 
    where concatleadingSlash = (<>) "/"  
          replaceHashToQuery = String.replace (String.Pattern "#") (String.Replacement "?")

parseSFOauthResponse' :: String -> Either String RouteQuery 
parseSFOauthResponse' l = match queryParams $ (concatleadingSlash <<< replaceHashToQuery) l
    where concatleadingSlash = (<>) "/"  
          replaceHashToQuery = String.replace (String.Pattern "#") (String.Replacement "?")

queryParams :: Match RouteQuery
queryParams = RouteQuery <$> (lit "" *> params)
 
runDecoder = decodeJSONWith decode

handleDecodeError = lmap (\err -> DecodeError $ show err)
