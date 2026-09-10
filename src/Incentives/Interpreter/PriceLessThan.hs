module Incentives.Interpreter.PriceLessThan where

import Incentives.CheckoutSummary (Price)
import Incentives.Interpreter (Interpreter (..))
import Incentives.Match (Match, match)
import Incentives.Rule (LineRule (PriceLessThan))

-- | Context is the current price; input is the exclusive upper bound.
priceLessThan :: Applicative m => Interpreter m Price Price (Match (LineRule, Price))
priceLessThan = Interpreter $ \observed threshold ->
  pure (match (observed < threshold) (PriceLessThan threshold, observed))
