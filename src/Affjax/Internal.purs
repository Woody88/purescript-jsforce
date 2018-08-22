module Affjax.Internal where

import Prelude

import Affjax (Response, ResponseFormatError, printResponseFormatError)
import Affjax.StatusCode (StatusCode(..))
import Control.Monad.Except (runExcept)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (fromMaybe)
import Data.Foldable (foldl)
import Foreign (MultipleErrors)
import Foreign.Class (class Decode, decode)
import Foreign.JSON (decodeJSONWith)
import Salesforce.Query.Types (QueryError(..))
import Salesforce.SObject.Types (SObjectError(..))
import Salesforce.Types (SalesforceErrorResponse(..), SalesforceErrorResponses)

type AffjaxSFError = { status   :: StatusCode 
                     , sfErrRes :: SalesforceErrorResponses
                     } 

class MapStatusCode a where
  mapStatusCode  :: AffjaxSFError -> a
  mapParserError :: String -> a

statusOk :: StatusCode -> Boolean
statusOk (StatusCode n) = n >= 200 && n < 304


decodeWithEitherError :: forall a b.
                   MapStatusCode a
                => Decode b
                => Response (Either ResponseFormatError String)
                -> Either a b
decodeWithEitherError res 
    | statusOk res.status = lmap mapParserError (runDecoderEither res.body)
    | otherwise           = do 
        sferr <- lmap mapParserError $ handleSFErrorEither res.body 
        Left $ mapStatusCode {status: res.status, sfErrRes: sferr}


decodeWithError :: forall a b.
                   MapStatusCode a
                => Decode b
                => Response String
                -> Either a b
decodeWithError res
  | statusOk res.status = lmap mapParserError (runDecoder res.body )
  | otherwise           = do 
    sferr <- lmap mapParserError $ handleSFError res.body 
    Left $ mapStatusCode {status: res.status, sfErrRes: sferr}

handleSFErrorEither :: Either ResponseFormatError String -> Either String SalesforceErrorResponses
handleSFErrorEither  eitherS = do
    s <- lmap printResponseFormatError eitherS
    handleSFError s

handleSFError :: String -> Either String SalesforceErrorResponses
handleSFError s = runDecoderImpl show s

runDecoderEither :: forall b. Decode b => Either ResponseFormatError String -> Either String b 
runDecoderEither eitherJ = do
    j <- lmap printResponseFormatError eitherJ
    runDecoder j 

runDecoder :: forall b. Decode b => String -> Either String b
runDecoder j = runDecoderImpl show j

runDecoderImpl :: forall e b. Decode b => (MultipleErrors -> e) -> String -> Either e b
runDecoderImpl f j = lmap f (runExcept $ decodeJSONWith decode j)

instance mapstatusCodeQueryError :: MapStatusCode QueryError where 
    mapStatusCode = sfErrorHoist QueryError <<< _.sfErrRes
    mapParserError = QueryParseError <<< show

instance mapstatusCodeSObject :: MapStatusCode SObjectError where 
    mapStatusCode = sfErrorHoist SObjectError <<< _.sfErrRes
    mapParserError = SObjectParseError <<< show

sfErrorHoist :: forall e. (String -> e) -> SalesforceErrorResponses -> e
sfErrorHoist f sferrs = f $ concatErr sferrs
    where
        concatErr errs = (foldl (\b (SFErrorResponse sferr) -> b <> sferr.errorCode <> ": " <> sferr.message <> (concatFields $ fromMaybe ["NoFields"] sferr.fields) <> "\n" ) mempty errs) 
        concatFields fields = foldl append "Fields: " fields 