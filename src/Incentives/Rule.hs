{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

module Incentives.Rule where

import Data.Text (Text)
import Incentives.Ast (Ast (Pure))
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
  deriving (Eq, Show)

-- Purchase facts are available in either target. Line facts require a line.
data Rule (target :: Target) where
  LineRule :: LineRule -> Rule 'Line
  PurchaseRule :: PurchaseRule -> Rule target
  AnyLine :: Ast (Rule 'Line) -> Rule 'Purchase
  EveryLine :: Ast (Rule 'Line) -> Rule 'Purchase

deriving instance Eq (Rule target)
deriving instance Show (Rule target)

line :: LineRule -> Ast (Rule 'Line)
line = Pure . LineRule

purchase :: PurchaseRule -> Ast (Rule target)
purchase = Pure . PurchaseRule

anyLine :: Ast (Rule 'Line) -> Ast (Rule 'Purchase)
anyLine = Pure . AnyLine

everyLine :: Ast (Rule 'Line) -> Ast (Rule 'Purchase)
everyLine = Pure . EveryLine
