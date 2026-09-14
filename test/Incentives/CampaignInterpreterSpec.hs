module Incentives.CampaignInterpreterSpec where

import Incentives.Campaign (Campaign, CampaignId, Incentive (..), Discount (..), LineIncentiveTarget (..), PurchaseIncentiveTarget (..), campaignId)
import qualified Incentives.Client.ProductDetails as Products
import Incentives.Client.Stub (stubProductDetailsClient, stubUserDetailsClient)
import qualified Incentives.ExampleCampaign as Examples
import Incentives.ExampleData
import Incentives.GrantedIncentive
import Incentives.Interpreter (evaluate, contramapContext)
import Incentives.Interpreter.Ast (interpretAst)
import Incentives.Interpreter.Campaign (interpretCampaigns)
import Incentives.Interpreter.Checkout (fullCheckoutInterpreter, checkoutLineInterpreters, checkoutPurchaseInterpreters)
import Incentives.Interpreter.ProductCategoryIs (productCategoryIs)
import Incentives.Interpreter.Rule (LineInterpreters (..), interpretLineRule, interpretPurchaseRule, interpretRule)
import Test.Hspec

spec :: Spec
spec = describe "Campaign interpretation" $ do
  it "evaluates the six existing campaigns with their correct recipients and attribution" $ do
    result <- evaluate (fullCheckoutInterpreter stubProductDetailsClient stubUserDetailsClient exampleNow) exampleCheckout
      [ Examples.lineRulesLineIncentive
      , Examples.purchaseRulesLineIncentive
      , Examples.lineRulesPurchaseIncentive
      , Examples.purchaseRulesPurchaseIncentive
      , Examples.hybridCampaign
      , Examples.bundleShippingIncentive
      ]
    result `shouldMatchList`
      ( attributed Examples.lineRulesLineIncentive sellingGrants
          ++ attributed Examples.purchaseRulesLineIncentive buyerGrants
          -- The clothing line makes the universal condition false.
          ++ attributed Examples.lineRulesPurchaseIncentive []
          ++ attributed Examples.purchaseRulesPurchaseIncentive shippingGrants
          ++ attributed Examples.hybridCampaign
            (sellingGrants ++ buyerGrants ++ shippingGrants
              ++ [ LineGrant bookLineId (Waive BoostingFee)
                 , LineGrant bookLineId (Reduce SellingFee (PercentOff 25))
                 ])
          ++ attributed Examples.bundleShippingIncentive shippingGrants
      )

  it "accepts a separately supplied category interpreter inside both line quantifiers" $ do
    let products = Products.ProductDetailsClient
          { Products.fetch = \_ -> fail "The replacement category interpreter must not fetch products"
          , Products.fetchBatch = \_ -> fail "The replacement category interpreter must not fetch products"
          }
        lineInterpreters = (checkoutLineInterpreters products stubUserDetailsClient exampleNow)
          { category = contramapContext (const "books") productCategoryIs }
        interpreter = interpretCampaigns (interpretAst (interpretRule
          (interpretLineRule lineInterpreters)
          (interpretPurchaseRule (checkoutPurchaseInterpreters stubUserDetailsClient exampleNow))))
    -- Treating clothing as books makes EveryLine succeed; AnyLine still uses
    -- the normal price interpreter and the original checkout line contexts.
    result <- evaluate interpreter exampleCheckout [Examples.lineRulesPurchaseIncentive]
    result `shouldBe` attributed Examples.lineRulesPurchaseIncentive shippingGrants

attributed :: Campaign -> [GrantedIncentive] -> [(CampaignId, GrantedIncentive)]
attributed campaign = map (\instruction -> (campaignId campaign, instruction))

sellingGrants, buyerGrants, shippingGrants :: [GrantedIncentive]
sellingGrants =
  [ LineGrant bookLineId (Waive SellingFee)
  , LineGrant clothingLineId (Waive SellingFee)
  ]
buyerGrants =
  [ LineGrant ident (Waive BuyerFee)
  | ident <- [bookLineId, electronicsLineId, clothingLineId]
  ]
shippingGrants = [PurchaseGrant (Waive ShippingCost)]
