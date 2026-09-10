module Incentives.Interpreter.CurrencyIs where

import Incentives.CheckoutSummary (Currency)
import Incentives.Interpreter (Interpreter (..))
import Incentives.Eligibility (Eligibility, fromBool)

-- | Context is the checkout currency; input is the expected currency.
currencyIs :: Applicative m => Interpreter m Currency Currency Eligibility
currencyIs = Interpreter $ \observed expected ->
  pure (fromBool (observed == expected))
