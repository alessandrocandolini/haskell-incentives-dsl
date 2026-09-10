module Incentives.Rule where

import Data.Text (Text)
import Numeric.Natural (Natural)
import Incentives.EligibilityExpr (EligibilityExpr (Check, AnyLine, EveryLine))
import Incentives.CheckoutSummary (Currency, Days, Price, ShippingProvider)

data Target = Line | Purchase

data LineRule
  = PriceLessThan Price
  | ProductCategoryIs Text
  | SellerAccountAgeGreaterThan Days
  deriving (Eq, Show)

data PurchaseRule
  = CurrencyIs Currency
  | ShippingProviderIs ShippingProvider
  | BuyerAccountAgeGreaterThan Days
  | DistinctSellerCountGreaterThan Natural
  deriving (Eq, Show)

data Rule
  = LineRule LineRule
  | PurchaseRule PurchaseRule
  deriving (Eq, Show)

line :: LineRule -> EligibilityExpr Rule
line = Check . LineRule

purchase :: PurchaseRule -> EligibilityExpr Rule
purchase = Check . PurchaseRule

-- Derived rule: repeated lines from one seller do not form a bundle.
isBundle :: EligibilityExpr Rule
isBundle = purchase (DistinctSellerCountGreaterThan 1)

anyLine :: EligibilityExpr Rule -> EligibilityExpr Rule
anyLine = AnyLine

everyLine :: EligibilityExpr Rule -> EligibilityExpr Rule
everyLine = EveryLine
