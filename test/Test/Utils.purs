module Test.Utils where 

import Prelude

import Data.Either (Either(..))
import Control.Monad.Except (runExcept)
import Foreign (MultipleErrors)
import Foreign.Class (class Encode, class Decode)
import Foreign.Generic (decodeJSON, encodeJSON)
import Test.Spec (Spec, it)
import Test.Spec.Assertions (shouldEqual)

testEncodeJSON 
  :: forall a
   . Encode a
  => Show a
  => Eq a
  => a
  -> String
  -> Spec Unit
testEncodeJSON original expected = do
  log' "can be converted to JSON"
    (show original) json
  it' "conversion to JSON match" (show original) expected $
    json `shouldEqual` expected
  where
    json = encodeJSON $ original
    format a b c = a <> "\n    " <> b <> "\n -> " <> c
    log' t a b = it (format t a b) $ pure unit
    it' a b c t = it (format a b $ show c) t

testDecodeJSON
  :: forall a
   . Show a
  => Decode a
  => Eq a
  => String
  -> Either MultipleErrors a
  -> Spec Unit
testDecodeJSON input expected = do
  it' "can be converted from JSON" input expected $
    decodeJSON' input `shouldEqual` expected
  where
    decodeJSON' = runExcept <<< decodeJSON
    format a b c = a <> "\n    " <> b <> "\n -> " <> c
    log' t a b = it (format t a b) $ pure unit
    it' a b c t = it (format a b $ show c) t

testJSON
  :: forall a
   . Encode a
  => Show a
  => Decode a
  => Eq a
  => a
  -> String
  -> Either MultipleErrors a
  -> Spec Unit
testJSON original input expected = do
  log' "can be converted to JSON"
    (show original) json
  it "can be converted back" $
    decodeJSON' json `shouldEqual` Right original
  it' "can be converted from JSON" input expected $
    decodeJSON' input `shouldEqual` expected
  where
    decodeJSON' = runExcept <<< decodeJSON
    json = encodeJSON $ original
    format a b c = a <> "\n    " <> b <> "\n -> " <> c
    log' t a b = it (format t a b) $ pure unit
    it' a b c t = it (format a b $ show c) t