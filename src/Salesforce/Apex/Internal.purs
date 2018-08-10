module Salesforce.Apex.Internal where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn5, runFn5)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Foreign (Foreign)
import Salesforce.Connection (RequestError(..))
import Salesforce.Types (Connection, Salesforce, salesforce)
import Simple.JSON as JSON

get :: forall response opts. JSON.ReadForeign response => String -> opts -> Salesforce RequestError response
get path opts = salesforce \conn -> do
  eitherF <- runGetImpl conn path opts 
  pure $ eitherF >>= decodeResponse

runGetImpl :: forall opts. Connection -> String -> opts -> Aff (Either RequestError Foreign)
runGetImpl conn path opts = fromEffectFnAff $ runFn5 getImpl conn path opts (Left <<< Error) Right


decodeResponse :: forall response. JSON.ReadForeign response => Foreign -> Either RequestError response
decodeResponse = bimap DecodeError identity <<< runExcept <<< JSON.read'

foreign import getImpl :: forall a b opts. Fn5 Connection a opts (a -> b) (Foreign -> b) (EffectFnAff b)