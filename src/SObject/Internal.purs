module Salesforce.SObject.Internal where 


import Prelude

import Control.Monad.Reader (class MonadReader)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Bifunctor (lmap)
import Data.Symbol (class IsSymbol)
import Data.Variant (SProxy(..), Variant, inj)
import Effect.Aff.Class (class MonadAff)
import Salesforce.Connection.Types (Connection)
import Salesforce.Internal (class HasNetwork, class HasNetworkType, class Keys, request)
import Salesforce.SObject.Types (class HasSObjectAPIName, SObjectEndpoint(..), SObjectError, SObjectId(..), SObjectInsertResult)
import Salesforce.Types (NetworkError)
import Salesforce.Util (EitherV)
import Type.Proxy (Proxy(..))
import Type.Row (class RowToList, type (+))
import Unsafe.Coerce (unsafeCoerce)
                                    
insert :: forall m r sobject list nt sname fields. 
    RowToList fields list
    => Keys list
    => IsSymbol sname
    => HasSObjectAPIName sobject sname
    => HasNetwork m (SObjectEndpoint sobject fields) nt 
    => HasNetworkType m nt
    => MonadReader Connection m
    => MonadAff m
    => Proxy sobject
    -> { | fields }
    -> m (EitherV (SObjectError + r) (SObjectId sobject))
insert _ rec = do 
    (res :: EitherV (SObjectError + r) SObjectInsertResult) <- sobjectImpl Proxy (Insert rec :: SObjectEndpoint sobject fields)
    pure $ (SObjectId <<< _.id) <$> res  

retrieve :: forall m r sobject list nt sname fields. 
    RowToList fields list
    => Keys list
    => IsSymbol sname
    => DecodeJson {| fields} 
    => HasSObjectAPIName sobject sname
    => HasNetwork m (SObjectEndpoint sobject fields) nt 
    => HasNetworkType m nt
    => MonadReader Connection m
    => MonadAff m
    => SObjectId sobject
    -> Proxy (Record fields)
    -> m (EitherV (SObjectError + r) (Record fields))
retrieve sobjId pxy = sobjectImpl pxy (Retrieve sobjId)

update :: forall m r sobject list nt sname fields. 
    RowToList fields list
    => Keys list
    => IsSymbol sname
    => HasSObjectAPIName sobject sname
    => HasNetwork m (SObjectEndpoint sobject fields) nt 
    => HasNetworkType m nt
    => MonadReader Connection m
    => MonadAff m
    => SObjectId sobject
    -> { | fields }
    -> m (EitherV (SObjectError + r) Unit)
update sobjId rec = sobjectImpl Proxy (Update sobjId rec)

sobjectImpl :: forall m sobject result fields nt r sname list.
    RowToList fields list 
    => Keys list
    => IsSymbol sname
    => DecodeJson result
    => HasSObjectAPIName sobject sname
    => HasNetwork m (SObjectEndpoint sobject fields) nt 
    => HasNetworkType m nt
    => MonadReader Connection m
    => MonadAff m
    => Proxy { | fields }
    -> SObjectEndpoint sobject fields
    -> m (EitherV (SObjectError + r) result)
sobjectImpl _ sobj = do
    json <- lmap sobjectError <$> request sobj
    pure $ json >>= (lmap sobjectParseError <<< decodeJson)

sobjectError :: forall r. NetworkError -> Variant (sobjectError :: NetworkError | r) 
sobjectError = inj (SProxy :: SProxy "sobjectError") 

sobjectParseError :: forall r. String -> Variant (sobjectParseError :: String | r) 
sobjectParseError = inj (SProxy :: SProxy "sobjectParseError") 


