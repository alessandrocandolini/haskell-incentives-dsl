module Incentives.Interpreter.ShippingProviderIs where

import Incentives.CheckoutSummary (ShippingProvider)
import Incentives.Interpreter (Interpreter (..))
import Incentives.Match (Match, match)
import Incentives.Rule (PurchaseRule (ShippingProviderIs))

-- | Context is the observed provider; input is the expected provider.
shippingProviderIs :: Applicative m => Interpreter m ShippingProvider ShippingProvider (Match (PurchaseRule, ShippingProvider))
shippingProviderIs = Interpreter $ \observed expected ->
  pure (match (observed == expected) (ShippingProviderIs expected, observed))
