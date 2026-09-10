{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

module Incentives.Rule where

import Data.Text (Text)
import Incentives.Ast (Ast (Pure))
import Incentives.CheckoutSummary (Currency, Price, ShippingProvider)
import Numeric.Natural (Natural)

data Scope = LineScope | PurchaseScope

newtype Days = Days Natural deriving (Eq, Ord, Show)

days :: Natural -> Days
days = Days

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

-- Purchase facts are available in either scope. Line facts require a line.
data Rule (scope :: Scope) where
  Line :: LineRule -> Rule 'LineScope
  Purchase :: PurchaseRule -> Rule scope
  AnyLine :: Ast (Rule 'LineScope) -> Rule 'PurchaseScope
  EveryLine :: Ast (Rule 'LineScope) -> Rule 'PurchaseScope

deriving instance Eq (Rule scope)
deriving instance Show (Rule scope)

line :: LineRule -> Ast (Rule 'LineScope)
line = Pure . Line

purchase :: PurchaseRule -> Ast (Rule scope)
purchase = Pure . Purchase

anyLine :: Ast (Rule 'LineScope) -> Ast (Rule 'PurchaseScope)
anyLine = Pure . AnyLine

everyLine :: Ast (Rule 'LineScope) -> Ast (Rule 'PurchaseScope)
everyLine = Pure . EveryLine
