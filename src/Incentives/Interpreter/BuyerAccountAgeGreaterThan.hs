module Incentives.Interpreter.BuyerAccountAgeGreaterThan where

import Incentives.Interpreter (Interpreter (..))
import Incentives.CheckoutSummary (Days)
import Incentives.Eligibility (Eligibility, fromBool)

-- | Context is the buyer's calculated age in days; input is the exclusive bound.
buyerAccountAgeGreaterThan :: Applicative m => Interpreter m Days Days Eligibility
buyerAccountAgeGreaterThan = Interpreter $ \observed threshold ->
  pure (fromBool (observed > threshold))
