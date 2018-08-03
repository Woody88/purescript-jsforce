module QueryBuild where
  

-- import Prelude

-- import Data.Foldable (intercalate)
-- import Data.Generic.Rep (class Generic, Constructor(..), Product(..), from)
-- import Data.Generic.Rep.Show (genericShow)
-- import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
-- import Effect (Effect)
-- import Effect.Console (log)



-- select :: {} 
-- where_ :: condition
-- from :: Table


-- select { name: 3, id: () }
--  where_ { name }
--  orderby {id}
--  limit 10 

-- -- class EncodeValue a where
-- --   encodeValue ∷ a → String

-- -- instance encodeValueString ∷ EncodeValue String where
-- --   encodeValue = id

-- -- instance encodeValueInt ∷ EncodeValue Int where
-- --   encodeValue = show

-- -- class EncodeFields a where
-- --   encodeFields :: a -> Array String

-- -- instance encodeFieldsProduct
-- --   ∷ (EncodeFields a, EncodeFields b)
-- --   ⇒ EncodeFields (Product a b) where

-- --   encodeFields (Product a b) = encodeFields a <> encodeFields b

-- -- instance encodeFieldsField
-- --   ∷ (EncodeValue a, IsSymbol name)
-- --   ⇒ EncodeFields (Field name a) where

-- --   encodeFields (Field a) =
-- --     [reflectSymbol (SProxy :: SProxy name) <> "=" <> encodeValue a]

-- -- buildQueryString
-- --   ∷ ∀ a l n.
-- --     Generic n (Constructor l (Rec a))
-- --   ⇒ (EncodeFields a)
-- --   ⇒ n
-- --   → String
-- -- buildQueryString n =
-- --   build <<< from $ n
-- --  where
-- --   build (Constructor (Rec fields)) = intercalate "&" <<< encodeFields $ fields

-- newtype Person =
--   Person
--     { name   ∷ String
--     , age    ∷ Int
--     }

-- instance showPerson :: Show Person where 
--     show = genericShow

-- derive instance genericPerson ∷ Generic Person _

-- joe ∷ Person
-- joe = Person { name: "joe", age: 10 }

-- -- main :: Effect Unit
-- -- main = do
-- --   log <<< buildQueryString $ joe