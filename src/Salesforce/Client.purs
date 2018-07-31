module Salesforce.Client where

import Prelude (($), (<<<))
import Prim.Row (class Union)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn5, runFn5)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Salesforce.Types (Client, Connection)
import Salesforce.Connection (ConnectionConfig)

data LoginError = Cancelled | Error String

type WindowSize
    = { width  :: Int
      , height :: Int 
      }

type LoginOptions 
    = ( size  :: WindowSize
      , scope :: Int 
      )

login ::  forall configs o. Union configs o LoginOptions => { | configs } -> Client -> Aff (Either LoginError Connection)
login opts client = fromEffectFnAff $ runFn5 login_ client opts (Left Cancelled) (Left <<< Error) Right

foreign import mkClient :: forall configs o. Union configs o ConnectionConfig => { | configs } -> Effect Client 
foreign import login_ :: forall configs o a. Union configs o LoginOptions =>  Fn5 Client { | configs } a (String -> a) (Connection -> a) (EffectFnAff a)
       