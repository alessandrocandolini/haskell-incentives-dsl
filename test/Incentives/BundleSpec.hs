{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}

module Incentives.BundleSpec where

import Data.Functor.Identity (runIdentity)
import Incentives.EligibilityExpr (EligibilityExpr)
import qualified Incentives.CheckoutSummary as Checkout
import Incentives.Eligibility (Eligibility (..))
import Incentives.ExampleData
import Incentives.Interpreter (evaluate)
import Incentives.Interpreter.DistinctSellerCountGreaterThan (distinctSellerCountGreaterThan)
import Incentives.Rule
import Test.Hspec

spec :: Spec
spec = describe "isBundle" $ do
  it "is pure rule data derived from distinct seller count" $
    (isBundle :: EligibilityExpr Rule)
      `shouldBe` purchase (DistinctSellerCountGreaterThan 1)

  it "rejects a single seller" $
    runIdentity (evaluate distinctSellerCountGreaterThan [establishedSellerId] 1)
      `shouldBe` NotEligible

  it "rejects multiple lines from the same seller" $
    runIdentity (evaluate distinctSellerCountGreaterThan [establishedSellerId, establishedSellerId, establishedSellerId] 1)
      `shouldBe` NotEligible

  it "accepts two distinct sellers, even with repeated lines" $
    runIdentity (evaluate distinctSellerCountGreaterThan [establishedSellerId, recentSellerId, establishedSellerId] 1)
      `shouldBe` Eligible

  it "accepts the example checkout with two distinct sellers" $
    runIdentity (evaluate distinctSellerCountGreaterThan (fmap Checkout.sellerId (Checkout.lines exampleCheckout)) 1)
      `shouldBe` Eligible

  it "compares the distinct count strictly against the supplied threshold" $
    runIdentity (evaluate distinctSellerCountGreaterThan [establishedSellerId, recentSellerId, establishedSellerId] 2)
      `shouldBe` NotEligible
