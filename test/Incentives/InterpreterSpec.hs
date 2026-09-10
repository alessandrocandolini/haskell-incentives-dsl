module Incentives.InterpreterSpec where

import Data.Functor.Identity (Identity, runIdentity)
import qualified Incentives.Client.ProductDetails as ProductDetails
import Incentives.CheckoutSummary
import Incentives.Eligibility (Eligibility (..))
import Incentives.ExampleData
import Incentives.Interpreter (Interpreter (..))
import Incentives.Interpreter.BuyerAccountAgeGreaterThan
import Incentives.Interpreter.CurrencyIs
import Incentives.Interpreter.PriceLessThan
import Incentives.Interpreter.ProductCategoryIs
import Incentives.Interpreter.SellerAccountAgeGreaterThan
import Incentives.Interpreter.ShippingProviderIs
import Incentives.Match
import Incentives.Rule
import Test.Hspec

spec :: Spec
spec = do
  describe "priceLessThan" $ do
    it "compares the supplied current price" $
      runIdentity (evaluate priceLessThan (currentPrice bookLine) priceThreshold)
        `shouldBe` Hit (PriceLessThan priceThreshold, cheapPrice)
    it "misses at the threshold" $
      runIdentity (evaluate priceLessThan cheapPrice cheapPrice)
        `shouldBe` Miss (PriceLessThan cheapPrice, cheapPrice)
    it "misses above the threshold" $
      runIdentity (evaluate priceLessThan expensivePrice priceThreshold)
        `shouldBe` Miss (PriceLessThan priceThreshold, expensivePrice)

  describe "currencyIs" $ do
    it "hits for the observed currency" $
      runIdentity (evaluate currencyIs (currency exampleCheckout) USD)
        `shouldBe` Hit (CurrencyIs USD, USD)
    it "retains expected and observed currencies on a miss" $
      runIdentity (evaluate currencyIs (currency exampleCheckout) GBP)
        `shouldBe` Miss (CurrencyIs GBP, USD)

  describe "shippingProviderIs" $ do
    it "hits for the observed provider" $
      runIdentity (evaluate shippingProviderIs (shippingProvider exampleShipping) USPS)
        `shouldBe` Hit (ShippingProviderIs USPS, USPS)
    it "retains expected and observed providers on a miss" $
      runIdentity (evaluate shippingProviderIs (shippingProvider exampleShipping) UPS)
        `shouldBe` Miss (ShippingProviderIs UPS, USPS)

  describe "productCategoryIs" $ do
    it "hits for the category from product details" $
      runIdentity (evaluate productCategoryIs (ProductDetails.category bookDetails) "books")
        `shouldBe` Hit (ProductCategoryIs "books", "books")
    it "misses a different category" $
      runIdentity (evaluate productCategoryIs (ProductDetails.category electronicsDetails) "books")
        `shouldBe` Miss (ProductCategoryIs "books", "electronics")

  describe "buyerAccountAgeGreaterThan" $ do
    it "hits above the threshold" $
      runIdentity (evaluate buyerAccountAgeGreaterThan establishedAccountAge (days 365))
        `shouldBe` Hit (BuyerAccountAgeGreaterThan (days 365), establishedAccountAge)
    it "misses at the threshold" $
      runIdentity (evaluate buyerAccountAgeGreaterThan (days 365) (days 365))
        `shouldBe` Miss (BuyerAccountAgeGreaterThan (days 365), days 365)
    it "misses below the threshold" $
      runIdentity (evaluate buyerAccountAgeGreaterThan recentAccountAge (days 365))
        `shouldBe` Miss (BuyerAccountAgeGreaterThan (days 365), recentAccountAge)

  describe "sellerAccountAgeGreaterThan" $ do
    it "hits above the threshold" $
      runIdentity (evaluate sellerAccountAgeGreaterThan establishedAccountAge (days 30))
        `shouldBe` Hit (SellerAccountAgeGreaterThan (days 30), establishedAccountAge)
    it "misses at the threshold" $
      runIdentity (evaluate sellerAccountAgeGreaterThan (days 30) (days 30))
        `shouldBe` Miss (SellerAccountAgeGreaterThan (days 30), days 30)
    it "misses below the threshold" $
      runIdentity (evaluate sellerAccountAgeGreaterThan recentAccountAge (days 30))
        `shouldBe` Miss (SellerAccountAgeGreaterThan (days 30), recentAccountAge)

  describe "Match projection" $ do
    it "projects a hit to Eligible" $
      eligibilityOf (Hit ()) `shouldBe` Eligible
    it "projects a miss to NotEligible" $
      eligibilityOf (Miss ()) `shouldBe` NotEligible
    it "can change the reason without changing the verdict" $
      fmap show (Miss (42 :: Int)) `shouldBe` Miss "42"

  describe "Interpreter output mapping" $ do
    it "projects a rule interpreter to eligibility" $
      runIdentity (evaluate (fmap eligibilityOf priceLessThan) cheapPrice priceThreshold)
        `shouldBe` Eligible
    it "preserves a miss through projection" $
      runIdentity (evaluate (fmap eligibilityOf priceLessThan) cheapPrice cheapPrice)
        `shouldBe` NotEligible
    it "satisfies functor identity for a rule interpreter" $
      runIdentity (evaluate (fmap id currencyIs) USD USD)
        `shouldBe` Hit (CurrencyIs USD, USD)
    it "satisfies functor composition for a rule interpreter" $ do
      let interpreter = currencyIs :: Interpreter Identity Currency Currency (Match (PurchaseRule, Currency))
      runIdentity (evaluate (fmap (show . eligibilityOf) interpreter) USD USD)
        `shouldBe` runIdentity (evaluate (fmap show (fmap eligibilityOf interpreter)) USD USD)
    it "preserves effects while mapping the output" $ do
      let interpreter :: Interpreter ((,) [Currency]) Currency Currency (Match ())
          interpreter = Interpreter $ \observed expected ->
            ([observed], match (observed == expected) ())
      evaluate (fmap eligibilityOf interpreter) USD GBP
        `shouldBe` ([USD], NotEligible)
