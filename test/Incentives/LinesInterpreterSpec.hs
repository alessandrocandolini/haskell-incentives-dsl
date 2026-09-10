{-# LANGUAGE OverloadedLists #-}

module Incentives.LinesInterpreterSpec where

import Data.Functor.Identity (Identity, runIdentity)
import qualified Incentives.CheckoutSummary as Checkout
import Incentives.CheckoutSummary (Line, LineId (..), Price, currentPrice, lineId)
import Incentives.Eligibility (Eligibility (..))
import Incentives.ExampleData
import Incentives.Interpreter (Interpreter (..))
import Incentives.Interpreter.Lines (interpretLines)
import Incentives.Interpreter.PriceLessThan (priceLessThan)
import Test.Hspec

linePriceLessThan :: Interpreter Identity Line Price Eligibility
linePriceLessThan = Interpreter $ \checkoutLine threshold ->
  evaluate priceLessThan (currentPrice checkoutLine) threshold

spec :: Spec
spec = describe "interpretLines" $ do
  it "preserves every line's identity, verdict, and checkout order" $
    runIdentity (evaluate (interpretLines linePriceLessThan) (Checkout.lines exampleCheckout) priceThreshold)
      `shouldBe` [(bookLineId, Eligible), (electronicsLineId, NotEligible), (clothingLineId, Eligible)]

  it "keeps separate lines for the same product distinct" $ do
    let secondBook = bookLine { lineId = LineId "another-book-line", currentPrice = expensivePrice }
    runIdentity (evaluate (interpretLines linePriceLessThan) [bookLine, secondBook] priceThreshold)
      `shouldBe` [(bookLineId, Eligible), (LineId "another-book-line", NotEligible)]

  it "handles a single ineligible line without dropping it" $
    runIdentity (evaluate (interpretLines linePriceLessThan) [electronicsLine] priceThreshold)
      `shouldBe` [(electronicsLineId, NotEligible)]

  it "preserves interpreter effects in line order" $ do
    let interpreter :: Interpreter ((,) [LineId]) Line Price Eligibility
        interpreter = Interpreter $ \checkoutLine threshold ->
          ([lineId checkoutLine], runIdentity (evaluate linePriceLessThan checkoutLine threshold))
    evaluate (interpretLines interpreter) [bookLine, electronicsLine] priceThreshold
      `shouldBe` ([bookLineId, electronicsLineId], [(bookLineId, Eligible), (electronicsLineId, NotEligible)])
