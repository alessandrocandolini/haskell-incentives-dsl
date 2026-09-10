module Incentives.Interpreter.BuyerAccountAgeGreaterThan where

import Incentives.Interpreter (Interpreter (..))
import Incentives.CheckoutSummary (Days)
import Incentives.Match (Match, match)
import Incentives.Rule (PurchaseRule (BuyerAccountAgeGreaterThan))

-- | Context is the buyer's calculated age in days; input is the exclusive bound.
buyerAccountAgeGreaterThan :: Applicative m => Interpreter m Days Days (Match (PurchaseRule, Days))
buyerAccountAgeGreaterThan = Interpreter $ \observed threshold ->
  pure (match (observed > threshold) (BuyerAccountAgeGreaterThan threshold, observed))
