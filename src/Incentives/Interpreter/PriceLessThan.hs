module Incentives.Interpreter.PriceLessThan where

import Incentives.CheckoutSummary (Price)
import Incentives.Interpreter (Interpreter (..))
import Incentives.Eligibility (Eligibility, fromBool)

-- | Context is the current price; input is the exclusive upper bound.
priceLessThan :: Applicative m => Interpreter m Price Price Eligibility
priceLessThan = Interpreter $ \observed threshold ->
  pure (fromBool (observed < threshold))
