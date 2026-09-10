module Incentives.Interpreter.Simple where

import Incentives.CheckoutSummary
import Incentives.Interpreter (Interpreter (..))
import Incentives.Match (Match, match)
import Incentives.Rule (LineRule (PriceLessThan), PurchaseRule (..))

-- Inputs are the parameters carried by each rule constructor.
-- Reasons temporarily retain the rule and observed value.
priceLessThan :: Applicative m => Interpreter m Product Price (Match (LineRule, Price))
priceLessThan = Interpreter $ \product threshold ->
  let observed = currentPrice product
  in pure (match (observed < threshold) (PriceLessThan threshold, observed))

currencyIs :: Applicative m => Interpreter m CheckoutSummary Currency (Match (PurchaseRule, Currency))
currencyIs = Interpreter $ \checkout expected ->
  let observed = currency checkout
  in pure (match (observed == expected) (CurrencyIs expected, observed))

shippingProviderIs :: Applicative m => Interpreter m ShippingSummary ShippingProvider (Match (PurchaseRule, ShippingProvider))
shippingProviderIs = Interpreter $ \shippingSummary expected ->
  let observed = shippingProvider shippingSummary
  in pure (match (observed == expected) (ShippingProviderIs expected, observed))
