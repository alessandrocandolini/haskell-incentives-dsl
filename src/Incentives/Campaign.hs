{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Incentives.Campaign where

import Data.Text (Text)
import Data.Time (UTCTime)
import Incentives.Ast (Ast)
import Incentives.CheckoutSummary (Price)
import Incentives.Rule (Rule, Scope (..))

newtype CampaignId = CampaignId Text deriving (Eq, Ord, Show)

data LineIncentiveTarget = SellingFee | BuyerFee | BoostingFee
  deriving (Eq, Ord, Show, Enum, Bounded)

data PurchaseIncentiveTarget = ShippingCost
  deriving (Eq, Ord, Show, Enum, Bounded)

-- Prototype values; validation and discount limits remain undecided.
data Discount = PercentOff Rational | AmountOff Price deriving (Eq, Show)

data Incentive target
  = Waive target
  | Reduce target Discount
  deriving (Eq, Show)

-- Composition preserves order and duplicates, with no binary grouping nodes.
newtype Offering (scope :: Scope) = Offering [OfferingNode scope]
  deriving (Eq, Show, Semigroup, Monoid)

data OfferingNode (scope :: Scope) where
  GrantLine :: Incentive LineIncentiveTarget -> OfferingNode 'LineScope
  GrantPurchase :: Incentive PurchaseIncentiveTarget -> OfferingNode 'PurchaseScope
  When :: Ast (Rule scope) -> Offering scope -> OfferingNode scope
  ForEachLine :: Offering 'LineScope -> OfferingNode 'PurchaseScope

deriving instance Eq (OfferingNode scope)
deriving instance Show (OfferingNode scope)

class IsIncentiveTarget target where
  type TargetScope target :: Scope
  grant :: Incentive target -> Offering (TargetScope target)

instance IsIncentiveTarget LineIncentiveTarget where
  type TargetScope LineIncentiveTarget = 'LineScope
  grant incentive = Offering [GrantLine incentive]

instance IsIncentiveTarget PurchaseIncentiveTarget where
  type TargetScope PurchaseIncentiveTarget = 'PurchaseScope
  grant incentive = Offering [GrantPurchase incentive]

when :: Ast (Rule scope) -> Offering scope -> Offering scope
when condition body = Offering [When condition body]

forEachLine :: Offering 'LineScope -> Offering 'PurchaseScope
forEachLine body = Offering [ForEachLine body]

data Campaign = Campaign
  { campaignId :: CampaignId
  , startsAt :: UTCTime
  , endsAt :: UTCTime
  , offering :: Offering 'PurchaseScope
  }
  deriving (Eq, Show)
