{-# LANGUAGE DataKinds #-}

module Incentives.Examples where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Time (UTCTime (..), fromGregorian)
import Incentives.Ast
import Incentives.Campaign
import Incentives.CheckoutSummary
import Incentives.Rule

exampleCheckout :: CheckoutSummary
exampleCheckout = CheckoutSummary
  { currency = USD
  , buyerId = UserId "buyer-1"
  , shipping = ShippingSummary USPS (amount 5)
  , products =
      Product (ProductId "1234") (UserId "seller-1") (amount 8) (amount 12)
        :| [Product (ProductId "5678") (UserId "seller-2") (amount 20) (amount 20)]
  }

cheapProductFees :: Campaign
cheapProductFees = Campaign
  { campaignId = CampaignId "cheap-product-fees"
  , startsAt = UTCTime (fromGregorian 2026 9 1) 0
  , endsAt = UTCTime (fromGregorian 2026 10 1) 0
  , offering = forEachLine $
      when cheap $
        grant (Waive SellingFee)
  }

usdProductFees :: Campaign
usdProductFees = cheapProductFees
  { campaignId = CampaignId "usd-product-fees"
  , offering = when (purchase (CurrencyIs USD)) $
      forEachLine (grant (Waive SellingFee))
  }

shippingIfAnyCheapLine :: Campaign
shippingIfAnyCheapLine = cheapProductFees
  { campaignId = CampaignId "shipping-if-any-cheap-line"
  , offering = when (anyLine cheap) $
      grant (Waive ShippingCost)
  }

shippingIfEveryLineIsCheap :: Campaign
shippingIfEveryLineIsCheap = cheapProductFees
  { campaignId = CampaignId "shipping-if-every-line-is-cheap"
  , offering = when (everyLine cheap) $
      grant (Waive ShippingCost)
  }

uspsShipping :: Campaign
uspsShipping = cheapProductFees
  { campaignId = CampaignId "usps-shipping"
  , offering = when (purchase (ShippingProviderIs USPS)) $
      grant (Waive ShippingCost)
  }

-- These leaves describe facts that will eventually require the supplied clients.
discountedBoostingForEstablishedUsers :: Campaign
discountedBoostingForEstablishedUsers = cheapProductFees
  { campaignId = CampaignId "established-users-boosting"
  , offering = forEachLine $
      when
        ( cheap
            .&&. purchase (BuyerAccountAgeGreaterThan (days 365))
            .&&. ( line (ProductCategoryIs "books")
                     .||. Not (line (SellerAccountAgeGreaterThan (days 30)))
                 )
        )
        (grant (Reduce BoostingFee (PercentOff 50)))
  }

cheap :: Ast (Rule 'LineScope)
cheap = line (PriceLessThan (amount 10))

lineBenefits :: Offering 'LineScope
lineBenefits =
  grant (Waive SellingFee)
    <> grant (Reduce BoostingFee (PercentOff 50))

shippingBenefits :: Offering 'PurchaseScope
shippingBenefits =
  when (anyLine cheap) $
    grant (Waive ShippingCost)

benefits :: Offering 'PurchaseScope
benefits =
  forEachLine (when cheap lineBenefits)
    <> shippingBenefits

usdBenefits :: Offering 'PurchaseScope
usdBenefits = when (purchase (CurrencyIs USD)) benefits

establishedBuyerBenefits :: Offering 'PurchaseScope
establishedBuyerBenefits =
  when (purchase (BuyerAccountAgeGreaterThan (days 365))) usdBenefits

hybridCampaign :: Campaign
hybridCampaign = Campaign
  { campaignId = CampaignId "september-benefits"
  , startsAt = UTCTime (fromGregorian 2026 9 1) 0
  , endsAt = UTCTime (fromGregorian 2026 10 1) 0
  , offering = when (purchase (CurrencyIs USD)) $
      forEachLine
        (when cheap $
           grant (Waive SellingFee)
             <> grant (Reduce BoostingFee (PercentOff 50))
        )
        <> when (anyLine cheap) (grant (Waive ShippingCost))
  }

independentLineBenefits :: Offering 'LineScope
independentLineBenefits =
  when cheap (grant (Waive SellingFee))
    <> when (line (ProductCategoryIs "books")) (grant (Waive BuyerFee))
