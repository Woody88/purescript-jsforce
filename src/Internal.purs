module Salesforce.Internal where 

import Prelude

import Affjax (Response, ResponseFormatError, printResponseFormatError)
import Affjax.StatusCode (StatusCode(..))
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader (class MonadReader)
import Control.Plus (empty)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson, (.:), (.:?))
import Data.Bifunctor (lmap)
import Data.Either (Either, either)
import Data.List(List,(:))
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Effect.Aff.Class (class MonadAff)
import Prim.RowList as RL
import Salesforce.Connection.Types (Connection)
import Salesforce.Types (NetworkError, kind NetworkType)
import Salesforce.Util (Url)
import Type.Data.RowList (RLProxy(..))

class Functor m <= HasNetworkType m (n :: NetworkType) | m -> n

class HasEndpoint sfapi where 
    endpointUrl :: Connection -> sfapi -> Url

-- | HasNetwork typclass which represent the request to salesforce
class HasNetwork m sfapi (n :: NetworkType) where 
    request :: 
        HasEndpoint sfapi  
        => HasNetworkType m n
        => MonadReader Connection m
        => MonadAff m
        => Applicative m
        => sfapi  
        -> m (Either NetworkError Json)

networkParseError :: String -> NetworkError
networkParseError message = {errorCode: "Network Parse Error", fields: empty, message} 

validateRequest :: Response (Either ResponseFormatError Json) -> Either NetworkError Json 
validateRequest {status: (StatusCode status), body} 
    | status >= 200 && status < 304 = either (throwError <<< networkParseError) (pure <<< identity) $ lmap printResponseFormatError body 
    | otherwise                     = throwError $ either (networkParseError <<< printResponseFormatError) decodeNetworkError body

decodeNetworkError :: Json -> NetworkError
decodeNetworkError json = do 
    let eitherNetworkErr = do
            obj       <- decodeJson json
            errorCode <- obj .: "errorCode"
            message   <- obj .: "message"
            fields    <- obj .:? "fields"
            (pure {errorCode, message, fields} :: Either String NetworkError)
    either networkParseError identity eitherNetworkErr


class Keys (xs :: RL.RowList) where
  keysImpl :: RLProxy xs -> List String

instance nilKeys :: Keys RL.Nil where
  keysImpl _ = mempty

instance consKeys ::
  ( IsSymbol name
  , Keys tail
  ) => Keys (RL.Cons name ty tail) where
  keysImpl _ = first : rest
    where
      first = reflectSymbol (SProxy :: SProxy name)
      rest = keysImpl (RLProxy :: RLProxy tail)
