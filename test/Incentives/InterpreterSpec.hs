module Incentives.InterpreterSpec where

import Data.Functor.Identity (Identity, runIdentity)
import qualified Incentives.Client.ProductDetails as ProductDetails
import Incentives.CheckoutSummary
import Incentives.Eligibility (Eligibility (..), fromBool)
import Incentives.ExampleData
import Incentives.Interpreter (Interpreter (..))
import Incentives.Interpreter.BuyerAccountAgeGreaterThan
import Incentives.Interpreter.CurrencyIs
import Incentives.Interpreter.PriceLessThan
import Incentives.Interpreter.ProductCategoryIs
import Incentives.Interpreter.SellerAccountAgeGreaterThan
import Incentives.Interpreter.ShippingProviderIs
import Test.Hspec

spec :: Spec
spec = do
  describe "priceLessThan" $ do
    it "compares the supplied current price" $
      runIdentity (evaluate priceLessThan (currentPrice bookLine) priceThreshold)
        `shouldBe` Eligible
    it "misses at the threshold" $
      runIdentity (evaluate priceLessThan cheapPrice cheapPrice)
        `shouldBe` NotEligible
    it "misses above the threshold" $
      runIdentity (evaluate priceLessThan expensivePrice priceThreshold)
        `shouldBe` NotEligible

  describe "currencyIs" $ do
    it "hits for the observed currency" $
      runIdentity (evaluate currencyIs (currency exampleCheckout) USD)
        `shouldBe` Eligible
    it "rejects a different currency" $
      runIdentity (evaluate currencyIs (currency exampleCheckout) GBP)
        `shouldBe` NotEligible

  describe "shippingProviderIs" $ do
    it "hits for the observed provider" $
      runIdentity (evaluate shippingProviderIs (shippingProvider exampleShipping) USPS)
        `shouldBe` Eligible
    it "rejects a different provider" $
      runIdentity (evaluate shippingProviderIs (shippingProvider exampleShipping) UPS)
        `shouldBe` NotEligible

  describe "productCategoryIs" $ do
    it "hits for the category from product details" $
      runIdentity (evaluate productCategoryIs (ProductDetails.category bookDetails) "books")
        `shouldBe` Eligible
    it "misses a different category" $
      runIdentity (evaluate productCategoryIs (ProductDetails.category electronicsDetails) "books")
        `shouldBe` NotEligible

  describe "buyerAccountAgeGreaterThan" $ do
    it "hits above the threshold" $
      runIdentity (evaluate buyerAccountAgeGreaterThan establishedAccountAge (days 365))
        `shouldBe` Eligible
    it "misses at the threshold" $
      runIdentity (evaluate buyerAccountAgeGreaterThan (days 365) (days 365))
        `shouldBe` NotEligible
    it "misses below the threshold" $
      runIdentity (evaluate buyerAccountAgeGreaterThan recentAccountAge (days 365))
        `shouldBe` NotEligible

  describe "sellerAccountAgeGreaterThan" $ do
    it "hits above the threshold" $
      runIdentity (evaluate sellerAccountAgeGreaterThan establishedAccountAge (days 30))
        `shouldBe` Eligible
    it "misses at the threshold" $
      runIdentity (evaluate sellerAccountAgeGreaterThan (days 30) (days 30))
        `shouldBe` NotEligible
    it "misses below the threshold" $
      runIdentity (evaluate sellerAccountAgeGreaterThan recentAccountAge (days 30))
        `shouldBe` NotEligible

  describe "Interpreter output mapping" $ do
    it "maps an eligible result" $
      runIdentity (evaluate (fmap (== Eligible) priceLessThan) cheapPrice priceThreshold)
        `shouldBe` True
    it "maps an ineligible result" $
      runIdentity (evaluate (fmap (== Eligible) priceLessThan) cheapPrice cheapPrice)
        `shouldBe` False
    it "satisfies functor identity for a rule interpreter" $
      runIdentity (evaluate (fmap id currencyIs) USD USD)
        `shouldBe` Eligible
    it "satisfies functor composition for a rule interpreter" $ do
      let interpreter = currencyIs :: Interpreter Identity Currency Currency Eligibility
      runIdentity (evaluate (fmap (show . (== Eligible)) interpreter) USD USD)
        `shouldBe` runIdentity (evaluate (fmap show (fmap (== Eligible) interpreter)) USD USD)
    it "preserves effects while mapping the output" $ do
      let interpreter :: Interpreter ((,) [Currency]) Currency Currency Eligibility
          interpreter = Interpreter $ \observed expected ->
            ([observed], fromBool (observed == expected))
      evaluate (fmap (== Eligible) interpreter) USD GBP
        `shouldBe` ([USD], False)
