module SalesforceMonadSpec where
  
-- import Prelude
-- import Data.Identity (Identity)
-- import Effect (Effect)
-- import Test.QuickCheck.Laws (A, checkLaws)
-- import Test.QuickCheck.Laws.Control as Control
-- import Test.QuickCheck.Laws.Data as Data
-- import Type.Proxy (Proxy(..), Proxy2(..))
-- import Salesforce.Types (SalesforceM(..))


-- checkIdentity ∷ Effect Unit
-- checkIdentity = checkLaws "SalesforceM" do
--   Data.checkFunctor prx2SalesforceM

-- --   Control.checkApply prx2Identity
-- --   Control.checkApplicative prx2Identity
-- --   Control.checkBind prx2Identity
-- --   Control.checkMonad prx2Identity
--   where
--   prxSalesforceM = Proxy ∷ Proxy (SalesforceM Connection)
--   prx2SalesforceM = Proxy2 ∷ Proxy2 SalesforceM
