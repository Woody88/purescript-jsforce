module Salesforce.SObject.Types where 

import Prelude

import Affjax as AX
import Affjax.RequestBody as RequestBody
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.Reader (ask)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.List as List
import Data.Newtype (class Newtype, unwrap)
import Data.String.Common as String
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Effect.Aff.Class (liftAff)
import Prim.RowList as RL
import Salesforce.Connection.Util (authorizationHeader, baseUrl)
import Salesforce.Internal (class HasEndpoint, class HasNetwork, class Keys, endpointUrl, keysImpl, validateRequest)
import Salesforce.Types (Affjax, NetworkError)
import Type.Data.RowList (RLProxy(..))

newtype SObjectId sobject = SObjectId String 

type SObjectInsertResult = { id      :: String 
                           , errors  :: Array String
                           , success :: Boolean
                           }

derive instance newtypeSObjectId :: Newtype (SObjectId sobject) _ 

class HasSObjectAPIName sobject (name :: Symbol) | sobject -> name

data SObjectEndpoint sobject (fields :: # Type) 
    = Retrieve (SObjectId sobject)
    | Update (SObjectId sobject) { | fields } 
    | Insert { | fields }
    | Delete (SObjectId sobject)

type SObjectError r = ( sobjectError      :: NetworkError
                      , sobjectParseError :: String 
                      | r
                      )

instance hasSObjectEndpoint :: (RL.RowToList fields list, Keys list, IsSymbol sname, HasSObjectAPIName sobject sname) => HasEndpoint (SObjectEndpoint sobject fields) where 
    endpointUrl conn sobjectEndpoint = do 
        let sobjectName    = reflectSymbol $ SProxy :: _ sname 
            sobjectBaseUrl = baseUrl conn <> "/sobject/" <> sobjectName 
            fieldList      = String.joinWith "," $ List.toUnfoldable $ keysImpl (RLProxy :: RLProxy list)
        
        case sobjectEndpoint of 
            Retrieve sid -> sobjectBaseUrl <> "/" <> getId sid <> "?fields=" <> fieldList
            Update sid _ -> sobjectBaseUrl <> "/" <> getId sid 
            Insert _     -> sobjectBaseUrl
            Delete sid   -> sobjectBaseUrl <> "/" <> getId sid 


instance hasSObjectNetwork :: EncodeJson { | fields } => HasNetwork m (SObjectEndpoint sobject fields) Affjax where 
    request sobjectEndpoint = do
        conn <- ask
        let
            url = endpointUrl conn sobjectEndpoint
            authHeader = authorizationHeader conn
            unitResponseBody res = res { body = const (encodeJson unit) <$> res.body }

        (pure <<< validateRequest) =<< case sobjectEndpoint of 
            Retrieve _ -> 
                liftAff $ AX.request $ AX.defaultRequest
                    { url = url
                    , method = Left GET
                    , responseFormat = ResponseFormat.json
                    , headers = [ RequestHeader "Authorization" authHeader ]
                    }

            Update _ rec -> do
                res <- liftAff $ AX.request $ AX.defaultRequest
                    { url = url
                    , method = Left PATCH
                    , headers = [ RequestHeader "Authorization" authHeader ]
                    , content = pure <<< RequestBody.json $ encodeJson rec
                    }
                pure $ unitResponseBody res
            Insert rec -> 
                liftAff $ AX.request $ AX.defaultRequest
                    { url = url
                    , method = Left POST
                    , responseFormat = ResponseFormat.json
                    , headers = [ RequestHeader "Authorization" authHeader ]
                    , content = pure <<< RequestBody.json $ encodeJson rec
                    }
            Delete _ -> do
                res <-  liftAff $ AX.request $ AX.defaultRequest
                    { url = url
                    , method = Left DELETE
                    , headers = [ RequestHeader "Authorization" authHeader ]
                    , responseFormat = ResponseFormat.ignore
                    }
                pure $ unitResponseBody res

getId :: forall sobject. SObjectId sobject -> String 
getId = unwrap
