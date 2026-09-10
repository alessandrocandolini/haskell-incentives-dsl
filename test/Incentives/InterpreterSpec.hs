module Incentives.InterpreterSpec where

import Data.Functor.Identity (Identity, runIdentity)
import qualified Data.List.NonEmpty as NonEmpty
import Incentives.CheckoutSummary
import Incentives.Eligibility (Eligibility (..))
import Incentives.Examples (exampleCheckout)
import Incentives.Interpreter (Interpreter (..))
import qualified Incentives.Interpreter.Examples as Examples
import Incentives.Interpreter.Simple
import Incentives.Match
import Incentives.Rule
import Test.Hspec

productFixture :: Product
productFixture = NonEmpty.head (products exampleCheckout)

spec :: Spec
spec = do
  describe "priceLessThan" $ do
    it "uses current price even when original price exceeds the threshold" $
      Examples.priceMatch `shouldBe` Hit (PriceLessThan (amount 10), amount 8)
    it "misses at the threshold" $
      runIdentity (evaluate priceLessThan productFixture (amount 8))
        `shouldBe` Miss (PriceLessThan (amount 8), amount 8)
    it "misses above the threshold" $
      runIdentity (evaluate priceLessThan productFixture (amount 7))
        `shouldBe` Miss (PriceLessThan (amount 7), amount 8)

  describe "currencyIs" $ do
    it "hits for the checkout currency" $
      Examples.currencyMatch `shouldBe` Hit (CurrencyIs USD, USD)
    it "retains expected and observed currencies on a miss" $
      runIdentity (evaluate currencyIs exampleCheckout GBP)
        `shouldBe` Miss (CurrencyIs GBP, USD)

  describe "shippingProviderIs" $ do
    it "hits for the actual shipping provider" $
      runIdentity (evaluate shippingProviderIs (shipping exampleCheckout) USPS)
        `shouldBe` Hit (ShippingProviderIs USPS, USPS)
    it "retains expected and observed providers on a miss" $
      Examples.shippingMatch `shouldBe` Miss (ShippingProviderIs UPS, USPS)

  describe "Match projection" $ do
    it "projects a hit to Eligible" $
      eligibilityOf (Hit ()) `shouldBe` Eligible
    it "projects a miss to NotEligible" $
      eligibilityOf (Miss ()) `shouldBe` NotEligible
    it "can change the reason without changing the verdict" $
      fmap show (Miss (42 :: Int)) `shouldBe` Miss "42"

  describe "Interpreter output mapping" $ do
    it "projects a rule interpreter to eligibility" $
      Examples.priceEligibility `shouldBe` Eligible
    it "preserves a miss through projection" $
      runIdentity (evaluate Examples.priceEligibilityInterpreter productFixture (amount 8))
        `shouldBe` NotEligible
    it "satisfies functor identity for a rule interpreter" $
      runIdentity (evaluate (fmap id currencyIs) exampleCheckout USD)
        `shouldBe` Examples.currencyMatch
    it "satisfies functor composition for a rule interpreter" $ do
      let interpreter = currencyIs :: Interpreter Identity CheckoutSummary Currency (Match (PurchaseRule, Currency))
      runIdentity (evaluate (fmap (show . eligibilityOf) interpreter) exampleCheckout USD)
        `shouldBe` runIdentity (evaluate (fmap show (fmap eligibilityOf interpreter)) exampleCheckout USD)
    it "preserves effects while mapping the output" $ do
      let interpreter :: Interpreter ((,) [Currency]) CheckoutSummary Currency (Match ())
          interpreter = Interpreter $ \checkout expected ->
            ([currency checkout], match (currency checkout == expected) ())
      evaluate (fmap eligibilityOf interpreter) exampleCheckout GBP
        `shouldBe` ([USD], NotEligible)
