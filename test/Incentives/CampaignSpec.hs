{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}

module Incentives.CampaignSpec where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Semigroup (sconcat)
import Incentives.Ast (Ast)
import Incentives.Campaign
import Incentives.CheckoutSummary (Currency (..), days)
import qualified Incentives.ExampleCampaign as Examples
import Incentives.ExampleData (priceThreshold)
import Incentives.Rule
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Gen, elements, forAll, listOf, oneof, sized)

cheap :: Ast (Rule 'Line)
cheap = line (PriceLessThan priceThreshold)

lineOffering :: Gen (Offering 'Line)
lineOffering = sized go
  where
    go n
      | n <= 0 = grant . Waive <$> elements ([minBound .. maxBound] :: [LineIncentiveTarget])
      | otherwise = oneof
          [ go 0
          , when cheap <$> go (n `div` 2)
          , sconcat <$> ((:|) <$> go 0 <*> listOf (go 0))
          ]

purchaseOffering :: Gen (Offering 'Purchase)
purchaseOffering = sized go
  where
    go n
      | n <= 0 = pure (grant (Waive ShippingCost))
      | otherwise = oneof
          [ go 0
          , forEachLine <$> lineOffering
          , when (purchase (CurrencyIs USD)) <$> go (n `div` 2)
          , sconcat <$> ((:|) <$> go 0 <*> listOf (go 0))
          ]

grantCount :: Offering target -> Int
grantCount (Offering nodes) = sum (fmap countNode nodes)
  where
    countNode :: OfferingNode t -> Int
    countNode (GrantLine _) = 1
    countNode (GrantPurchase _) = 1
    countNode (When _ body) = grantCount body
    countNode (ForEachLine body) = grantCount body

compositionLaws :: Gen (Offering target) -> Spec
compositionLaws generator = do
  prop "composition is associative" $
    forAll generator $ \a -> forAll generator $ \b -> forAll generator $ \c ->
      (a <> b) <> c == a <> (b <> c)
  prop "composition preserves all declared grants" $
    forAll generator $ \a -> forAll generator $ \b ->
      grantCount (a <> b) == grantCount a + grantCount b

spec :: Spec
spec = describe "Composable offerings" $ do
  describe "line target" $ compositionLaws lineOffering
  describe "purchase target" $ compositionLaws purchaseOffering

  it "preserves grant order and duplicates" $
    (grant (Waive BuyerFee) <> grant (Waive SellingFee) <> grant (Waive BuyerFee))
      `shouldBe` Offering
        [ GrantLine (Waive BuyerFee)
        , GrantLine (Waive SellingFee)
        , GrantLine (Waive BuyerFee)
        ]

  it "keeps separate conditions attached to their respective grants" $
    (when cheap (grant (Waive SellingFee))
      <> when (line (ProductCategoryIs "books")) (grant (Waive BuyerFee)))
      `shouldBe` Offering
        [ When cheap (Offering [GrantLine (Waive SellingFee)])
        , When (line (ProductCategoryIs "books")) (Offering [GrantLine (Waive BuyerFee)])
        ]

  it "retains nested gates when a condition wraps a reusable fragment" $
    when (purchase (CurrencyIs USD)) (offering Examples.hybridCampaign)
      `shouldBe` Offering
        [When (purchase (CurrencyIs USD)) (offering Examples.hybridCampaign)]

  it "the hybrid reuses all four campaign bodies under one shared condition" $ do
    let Offering expected = sconcat
          [ offering Examples.lineRulesLineIncentive
          , offering Examples.purchaseRulesLineIncentive
          , offering Examples.lineRulesPurchaseIncentive
          , offering Examples.purchaseRulesPurchaseIncentive
          ]
    case offering Examples.hybridCampaign of
      Offering (When condition (Offering nodes) :| []) -> do
        condition `shouldBe` purchase (BuyerAccountAgeGreaterThan (days 30))
        take (length expected) (NonEmpty.toList nodes) `shouldBe` NonEmpty.toList expected
        length nodes `shouldBe` length expected + 1
      _ -> expectationFailure "Expected one shared gate around the hybrid body"

  it "all five campaigns retain their grants through conditions and iteration" $
    map (grantCount . offering)
      [ Examples.lineRulesLineIncentive
      , Examples.purchaseRulesLineIncentive
      , Examples.lineRulesPurchaseIncentive
      , Examples.purchaseRulesPurchaseIncentive
      , Examples.hybridCampaign
      ] `shouldBe` [1, 1, 1, 1, 6]
