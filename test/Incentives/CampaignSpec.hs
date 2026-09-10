{-# LANGUAGE DataKinds #-}

module Incentives.CampaignSpec where

import Incentives.Campaign
import Incentives.CheckoutSummary
import qualified Incentives.Examples as Examples
import Incentives.Rule
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Gen, elements, forAll, listOf, oneof, sized)

lineOffering :: Gen (Offering 'LineScope)
lineOffering = sized go
  where
    go n
      | n <= 0 = grant . Waive <$> elements ([minBound .. maxBound] :: [LineIncentiveTarget])
      | otherwise = oneof
          [ go 0
          , when Examples.cheap <$> go (n `div` 2)
          , mconcat <$> listOf (go 0)
          ]

purchaseOffering :: Gen (Offering 'PurchaseScope)
purchaseOffering = sized go
  where
    go n
      | n <= 0 = pure (grant (Waive ShippingCost))
      | otherwise = oneof
          [ go 0
          , forEachLine <$> lineOffering
          , when (purchase (CurrencyIs USD)) <$> go (n `div` 2)
          , mconcat <$> listOf (go 0)
          ]

compositionLaws :: (Eq a, Show a, Monoid a) => Gen a -> Spec
compositionLaws generator = do
  prop "composition is associative" $
    forAll generator $ \a -> forAll generator $ \b -> forAll generator $ \c ->
      (a <> b) <> c == a <> (b <> c)
  prop "empty offering is a left identity" $
    forAll generator $ \body -> mempty <> body == body
  prop "empty offering is a right identity" $
    forAll generator $ \body -> body <> mempty == body

spec :: Spec
spec = describe "Composable offerings" $ do
  describe "line scope" $ compositionLaws lineOffering
  describe "purchase scope" $ compositionLaws purchaseOffering

  it "keeps a shared condition around both line and purchase grants" $
    Examples.usdBenefits `shouldBe`
      Offering
        [ When (purchase (CurrencyIs USD))
            (Offering
              [ ForEachLine (Offering [When Examples.cheap Examples.lineBenefits])
              , When (anyLine Examples.cheap)
                  (Offering [GrantPurchase (Waive ShippingCost)])
              ])
        ]

  it "reusable fragments produce the same data as an inline hybrid campaign" $
    offering Examples.hybridCampaign `shouldBe` Examples.usdBenefits

  it "preserves grant order and duplicates" $
    (grant (Waive BuyerFee) <> Examples.lineBenefits <> grant (Waive BuyerFee))
      `shouldBe` Offering
        [ GrantLine (Waive BuyerFee)
        , GrantLine (Waive SellingFee)
        , GrantLine (Reduce BoostingFee (PercentOff 50))
        , GrantLine (Waive BuyerFee)
        ]

  it "keeps separate conditions attached to their respective grants" $
    Examples.independentLineBenefits `shouldBe` Offering
      [ When Examples.cheap (Offering [GrantLine (Waive SellingFee)])
      , When (line (ProductCategoryIs "books")) (Offering [GrantLine (Waive BuyerFee)])
      ]

  it "retains nested gates when another condition wraps a reusable fragment" $
    Examples.establishedBuyerBenefits `shouldBe` Offering
      [ When (purchase (BuyerAccountAgeGreaterThan (days 365))) Examples.usdBenefits
      ]
