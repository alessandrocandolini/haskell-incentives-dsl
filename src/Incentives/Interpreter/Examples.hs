module Incentives.Interpreter.Examples where

import Data.Functor.Identity (Identity, runIdentity)
import qualified Data.List.NonEmpty as NonEmpty
import Incentives.CheckoutSummary
import Incentives.Eligibility (Eligibility)
import Incentives.Examples (exampleCheckout)
import Incentives.Interpreter (Interpreter (..))
import Incentives.Interpreter.Simple
import Incentives.Match (Match, eligibilityOf)
import Incentives.Rule (LineRule, PurchaseRule)

priceMatch :: Match (LineRule, Price)
priceMatch = runIdentity $
  evaluate priceLessThan (NonEmpty.head (products exampleCheckout)) (amount 10)

currencyMatch :: Match (PurchaseRule, Currency)
currencyMatch = runIdentity $
  evaluate currencyIs exampleCheckout USD

shippingMatch :: Match (PurchaseRule, ShippingProvider)
shippingMatch = runIdentity $
  evaluate shippingProviderIs (shipping exampleCheckout) UPS

priceEligibilityInterpreter :: Interpreter Identity Product Price Eligibility
priceEligibilityInterpreter = fmap eligibilityOf priceLessThan

priceEligibility :: Eligibility
priceEligibility = runIdentity $
  evaluate priceEligibilityInterpreter (NonEmpty.head (products exampleCheckout)) (amount 10)
