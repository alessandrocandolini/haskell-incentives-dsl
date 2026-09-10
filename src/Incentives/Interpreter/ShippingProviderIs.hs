module Incentives.Interpreter.ShippingProviderIs where

import Incentives.CheckoutSummary (ShippingProvider)
import Incentives.Interpreter (Interpreter (..))
import Incentives.Eligibility (Eligibility, fromBool)

-- | Context is the observed provider; input is the expected provider.
shippingProviderIs :: Applicative m => Interpreter m ShippingProvider ShippingProvider Eligibility
shippingProviderIs = Interpreter $ \observed expected ->
  pure (fromBool (observed == expected))
