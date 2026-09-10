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
import Incentives.Rule (Rule, Target (..))

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
newtype Offering (target :: Target) = Offering [OfferingNode target]
  deriving (Eq, Show, Semigroup, Monoid)

data OfferingNode (target :: Target) where
  GrantLine :: Incentive LineIncentiveTarget -> OfferingNode 'Line
  GrantPurchase :: Incentive PurchaseIncentiveTarget -> OfferingNode 'Purchase
  When :: Ast (Rule target) -> Offering target -> OfferingNode target
  ForEachLine :: Offering 'Line -> OfferingNode 'Purchase

deriving instance Eq (OfferingNode target)
deriving instance Show (OfferingNode target)

class IsIncentiveTarget target where
  type TargetOf target :: Target
  grant :: Incentive target -> Offering (TargetOf target)

instance IsIncentiveTarget LineIncentiveTarget where
  type TargetOf LineIncentiveTarget = 'Line
  grant incentive = Offering [GrantLine incentive]

instance IsIncentiveTarget PurchaseIncentiveTarget where
  type TargetOf PurchaseIncentiveTarget = 'Purchase
  grant incentive = Offering [GrantPurchase incentive]

when :: Ast (Rule target) -> Offering target -> Offering target
when condition body = Offering [When condition body]

forEachLine :: Offering 'Line -> Offering 'Purchase
forEachLine body = Offering [ForEachLine body]

data Campaign = Campaign
  { campaignId :: CampaignId
  , startsAt :: UTCTime
  , endsAt :: UTCTime
  , offering :: Offering 'Purchase
  }
  deriving (Eq, Show)
