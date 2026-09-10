module Incentives.GrantedIncentive where

import Incentives.Campaign (Incentive, LineIncentiveTarget, PurchaseIncentiveTarget)
import Incentives.CheckoutSummary (LineId)

-- Instructions for the caller, not changes to checkout amounts.
data GrantedIncentive
  = LineGrant LineId (Incentive LineIncentiveTarget)
  | PurchaseGrant (Incentive PurchaseIncentiveTarget)
  deriving (Eq, Show)
