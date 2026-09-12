{-# LANGUAGE DataKinds #-}

module Incentives.ExampleCampaign where

import Incentives.Ast
import Incentives.Campaign
import Incentives.CheckoutSummary (Currency (..), ShippingProvider (..), days)
import Incentives.ExampleData (campaignEndsAt, campaignStartsAt, priceThreshold)
import Incentives.Rule

-- A: line conditions govern a line incentive.
lineRulesLineIncentive :: Campaign
lineRulesLineIncentive = Campaign
  { campaignId = CampaignId "line-rules-line-incentive"
  , startsAt = campaignStartsAt
  , endsAt = campaignEndsAt
  , offering = forEachLine $
      when
        ( line (PriceLessThan priceThreshold)
            .&&. ( line (ProductCategoryIs "books")
                     .||. Not (line (SellerAccountAgeGreaterThan (days 365)))
                 )
        )
        (grant (Waive SellingFee))
  }

-- B: purchase conditions govern an incentive for every line.
purchaseRulesLineIncentive :: Campaign
purchaseRulesLineIncentive = Campaign
  { campaignId = CampaignId "purchase-rules-line-incentive"
  , startsAt = campaignStartsAt
  , endsAt = campaignEndsAt
  , offering = when
      ( purchase (CurrencyIs USD)
          .&&. purchase (BuyerAccountAgeGreaterThan (days 365))
      )
      (forEachLine (grant (Waive BuyerFee)))
  }

-- C: quantified line conditions govern a purchase incentive.
lineRulesPurchaseIncentive :: Campaign
lineRulesPurchaseIncentive = Campaign
  { campaignId = CampaignId "line-rules-purchase-incentive"
  , startsAt = campaignStartsAt
  , endsAt = campaignEndsAt
  , offering = when
      ( anyLine
          (line (PriceLessThan priceThreshold) .&&. line (ProductCategoryIs "books"))
          .&&. everyLine
            ( line (SellerAccountAgeGreaterThan (days 30))
                .||. line (ProductCategoryIs "books")
            )
      )
      (grant (Reduce ShippingCost (PercentOff 50)))
  }

-- D: purchase conditions govern a purchase incentive.
purchaseRulesPurchaseIncentive :: Campaign
purchaseRulesPurchaseIncentive = Campaign
  { campaignId = CampaignId "purchase-rules-purchase-incentive"
  , startsAt = campaignStartsAt
  , endsAt = campaignEndsAt
  , offering = when
      ( purchase (CurrencyIs USD)
          .&&. ( purchase (ShippingProviderIs USPS)
                   .||. purchase (ShippingProviderIs UPS)
               )
      )
      (grant (Waive ShippingCost))
  }

-- E: reuse all four bodies under one campaign, plus mixed-scope line benefits.
hybridCampaign :: Campaign
hybridCampaign = Campaign
  { campaignId = CampaignId "hybrid-incentives"
  , startsAt = campaignStartsAt
  , endsAt = campaignEndsAt
  , offering = when (purchase (BuyerAccountAgeGreaterThan (days 30))) $
      offering lineRulesLineIncentive
        <> offering purchaseRulesLineIncentive
        <> offering lineRulesPurchaseIncentive
        <> offering purchaseRulesPurchaseIncentive
        <> forEachLine
          (when
            (line (ProductCategoryIs "books") .&&. purchase (CurrencyIs USD))
            (grant (Waive BoostingFee) <> grant (Reduce SellingFee (PercentOff 25))))
  }

-- F: a derived purchase rule governs free shipping.
bundleShippingIncentive :: Campaign
bundleShippingIncentive = Campaign
  { campaignId = CampaignId "bundle-shipping-incentive"
  , startsAt = campaignStartsAt
  , endsAt = campaignEndsAt
  , offering = when isBundle (grant (Waive ShippingCost))
  }
