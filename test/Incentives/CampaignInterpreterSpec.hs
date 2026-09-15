module Incentives.CampaignInterpreterSpec where

import Data.Functor.Contravariant (contramap)
import Incentives.Ast (Ast (..))
import Incentives.Campaign (Campaign, CampaignId, Incentive (..), Discount (..), LineIncentiveTarget (..), PurchaseIncentiveTarget (..), campaignId)
import Incentives.CheckoutSummary (days)
import qualified Incentives.Client.ProductDetails as Products
import Incentives.Client.Stub (stubProductDetailsClient, stubUserDetailsClient)
import qualified Incentives.ExampleCampaign as Examples
import Incentives.ExampleData
import Incentives.Eligibility (Eligibility (..))
import qualified Incentives.Eligibility as Eligibility
import Incentives.Fetch (runFetch)
import Incentives.GrantedIncentive
import Incentives.Interpreter (evaluate, pack, unpack)
import Incentives.Interpreter.Ast (interpretAst, minimumWitness)
import Incentives.Interpreter.Campaign (CampaignResult (..), interpretCampaigns)
import Incentives.Interpreter.Checkout (fullCheckoutInterpreter, twoStageCheckoutInterpreter, checkoutLineInterpreters, checkoutPurchaseInterpreters)
import Incentives.Interpreter.ProductCategoryIs (productCategoryIs)
import Incentives.Interpreter.Rule (LineInterpreters (..), EvaluatedRule (..), interpretLineRule, interpretPurchaseRule, interpretRule, ruleEligibility)
import Incentives.Rule (LineRule (..))
import Test.Hspec

spec :: Spec
spec = describe "Campaign interpretation" $ do
  it "evaluates the six existing campaigns with their correct recipients and attribution" $ do
    result <- evaluate (fullCheckoutInterpreter stubProductDetailsClient stubUserDetailsClient exampleNow) exampleCheckout
      exampleCampaigns
    attributedResults result `shouldMatchList`
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

  it "agrees with the full interpreter on grants and gate verdicts across all six campaigns" $ do
    full <- evaluate (fullCheckoutInterpreter stubProductDetailsClient stubUserDetailsClient exampleNow)
      exampleCheckout exampleCampaigns
    staged <- runFetch stubProductDetailsClient stubUserDetailsClient $ evaluate (twoStageCheckoutInterpreter exampleNow)
      exampleCheckout exampleCampaigns
    let decisions result =
          ( resultCampaignId result
          , grantedIncentives result
          , [(recipient, Eligibility.evaluate (ruleEligibility <$> tree)) | (recipient, tree) <- evaluatedConditions result]
          )
    map decisions staged `shouldBe` map decisions full

  it "accepts a separately supplied category interpreter inside both line quantifiers" $ do
    let products = Products.ProductDetailsClient
          { Products.fetchBatch = \_ -> fail "The replacement category interpreter must not fetch products"
          }
        lineInterpreters = (checkoutLineInterpreters products stubUserDetailsClient exampleNow)
          { category = unpack $ contramap (const "books") $ pack productCategoryIs }
        interpreter = interpretCampaigns (minimumWitness ruleEligibility <$> interpretAst (interpretRule
          (interpretLineRule lineInterpreters)
          (interpretPurchaseRule (checkoutPurchaseInterpreters stubUserDetailsClient exampleNow))))
    -- Treating clothing as books makes EveryLine succeed; AnyLine still uses
    -- the normal price interpreter and the original checkout line contexts.
    result <- evaluate interpreter exampleCheckout [Examples.lineRulesPurchaseIncentive]
    attributedResults result `shouldBe` attributed Examples.lineRulesPurchaseIncentive
      [PurchaseGrant (Reduce ShippingCost (PercentOff 50))]

  it "returns the decisive evaluated line checks even when a purchase grant is rejected" $ do
    result <- evaluate (fullCheckoutInterpreter stubProductDetailsClient stubUserDetailsClient exampleNow)
      exampleCheckout [Examples.lineRulesPurchaseIncentive]
    result `shouldBe`
      [ CampaignResult (campaignId Examples.lineRulesPurchaseIncentive) []
          [ (Nothing, Or
              (Pure (EvaluatedLine clothingLineId (SellerAccountAgeGreaterThan (days 30)) NotEligible))
              (Pure (EvaluatedLine clothingLineId (ProductCategoryIs "books") NotEligible)))
          ]
      ]

exampleCampaigns :: [Campaign]
exampleCampaigns =
  [ Examples.lineRulesLineIncentive
  , Examples.purchaseRulesLineIncentive
  , Examples.lineRulesPurchaseIncentive
  , Examples.purchaseRulesPurchaseIncentive
  , Examples.hybridCampaign
  , Examples.bundleShippingIncentive
  ]

attributedResults :: [CampaignResult] -> [(CampaignId, GrantedIncentive)]
attributedResults results =
  [ (resultCampaignId result, incentive)
  | result <- results
  , incentive <- grantedIncentives result
  ]

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
