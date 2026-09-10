module Incentives.Interpreter.SellerAccountAgeGreaterThan where

import Incentives.Interpreter (Interpreter (..))
import Incentives.CheckoutSummary (Days)
import Incentives.Eligibility (Eligibility, fromBool)

-- | Context is the seller's calculated age in days; input is the exclusive bound.
sellerAccountAgeGreaterThan :: Applicative m => Interpreter m Days Days Eligibility
sellerAccountAgeGreaterThan = Interpreter $ \observed threshold ->
  pure (fromBool (observed > threshold))
