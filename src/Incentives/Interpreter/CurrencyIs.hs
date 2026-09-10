module Incentives.Interpreter.CurrencyIs where

import Incentives.CheckoutSummary (Currency)
import Incentives.Interpreter (Interpreter (..))
import Incentives.Match (Match, match)
import Incentives.Rule (PurchaseRule (CurrencyIs))

-- | Context is the checkout currency; input is the expected currency.
currencyIs :: Applicative m => Interpreter m Currency Currency (Match (PurchaseRule, Currency))
currencyIs = Interpreter $ \observed expected ->
  pure (match (observed == expected) (CurrencyIs expected, observed))
