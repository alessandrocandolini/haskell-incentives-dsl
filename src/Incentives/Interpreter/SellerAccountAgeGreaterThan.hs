module Incentives.Interpreter.SellerAccountAgeGreaterThan where

import Incentives.Interpreter (Interpreter (..))
import Incentives.CheckoutSummary (Days)
import Incentives.Match (Match, match)
import Incentives.Rule (LineRule (SellerAccountAgeGreaterThan))

-- | Context is the seller's calculated age in days; input is the exclusive bound.
sellerAccountAgeGreaterThan :: Applicative m => Interpreter m Days Days (Match (LineRule, Days))
sellerAccountAgeGreaterThan = Interpreter $ \observed threshold ->
  pure (match (observed > threshold) (SellerAccountAgeGreaterThan threshold, observed))
