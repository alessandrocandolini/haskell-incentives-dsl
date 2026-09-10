{-# LANGUAGE OverloadedLists #-}

module Incentives.EligibilityExprSpec where

import Data.Functor.Foldable (embed, project)
import Data.Functor.Identity (runIdentity)
import qualified Incentives.CheckoutSummary as Checkout
import Incentives.CheckoutSummary (Currency (..), Price)
import Incentives.Eligibility (Eligibility (..), fromBool)
import Incentives.EligibilityExpr
import Incentives.Evaluation
import Incentives.ExampleData
import Incentives.Interpreter (Interpreter (..))
import Incentives.Interpreter.CurrencyIs (currencyIs)
import Incentives.Interpreter.EligibilityExpr
import Incentives.Interpreter.PriceLessThan (priceLessThan)
import Incentives.Rule (Rule (..), LineRule (..), PurchaseRule (..), line, purchase)
import Test.Hspec

data Observed = ObservedPrice Price | ObservedCurrency Currency
  deriving (Eq, Show)

data TestError = MissingLine Rule | UnsupportedRule Rule
  deriving (Eq, Show)

-- Only local price and currency rules are wired for this structural experiment.
primitive :: Interpreter (Either TestError) Context Rule (Observed, Eligibility)
primitive = Interpreter $ \context rule -> case (context, rule) of
  (LineContext _ checkoutLine, LineRule (PriceLessThan threshold)) ->
    let observed = Checkout.currentPrice checkoutLine
    in (,) (ObservedPrice observed) <$> evaluate priceLessThan observed threshold
  (PurchaseContext _, LineRule _) -> Left (MissingLine rule)
  (_, PurchaseRule (CurrencyIs expected)) ->
    let observed = Checkout.currency (checkoutOf context)
    in (,) (ObservedCurrency observed) <$> evaluate currencyIs observed expected
  _ -> Left (UnsupportedRule rule)

interpret :: Context -> EligibilityExpr Rule -> Either TestError (Evaluation Observed Rule)
interpret = evaluate (interpretEligibilityExpr primitive)

cheap :: EligibilityExpr Rule
cheap = line (PriceLessThan priceThreshold)

usd :: EligibilityExpr Rule
usd = purchase (CurrencyIs USD)

priceLeaf :: Price -> Eligibility -> Evaluation Observed Rule
priceLeaf observed = Checked (ObservedPrice observed) (LineRule (PriceLessThan priceThreshold))

currencyLeaf :: Evaluation Observed Rule
currencyLeaf = Checked (ObservedCurrency USD) (PurchaseRule (CurrencyIs USD)) Eligible

spec :: Spec
spec = do
  describe "EligibilityExpr interpretation" $ do
    it "retains observed purchase data, the primitive rule, and its verdict" $
      interpret (PurchaseContext exampleCheckout) usd `shouldBe` Right currencyLeaf

    it "retains a failed line predicate" $
      interpret (LineContext exampleCheckout electronicsLine) cheap
        `shouldBe` Right (priceLeaf expensivePrice NotEligible)

    it "keeps Boolean structure and purchase access within a line context" $
      interpret (LineContext exampleCheckout electronicsLine) (Or cheap (Not usd))
        `shouldBe` Right (EvaluatedOr (priceLeaf expensivePrice NotEligible) (EvaluatedNot currencyLeaf))

    it "retains every per-line subtree in AnyLine, including misses after a hit" $ do
      let expected = EvaluatedAnyLine
            [ (bookLineId, EvaluatedAnd (priceLeaf cheapPrice Eligible) currencyLeaf)
            , (electronicsLineId, EvaluatedAnd (priceLeaf expensivePrice NotEligible) currencyLeaf)
            , (clothingLineId, EvaluatedAnd (priceLeaf cheapPrice Eligible) currencyLeaf)
            ]
      interpret (PurchaseContext exampleCheckout) (AnyLine (And cheap usd))
        `shouldBe` Right expected
      collapse expected `shouldBe` Eligible

    it "retains every per-line subtree in EveryLine, including hits after a miss" $ do
      let expected = EvaluatedEveryLine
            [ (bookLineId, priceLeaf cheapPrice Eligible)
            , (electronicsLineId, priceLeaf expensivePrice NotEligible)
            , (clothingLineId, priceLeaf cheapPrice Eligible)
            ]
      interpret (PurchaseContext exampleCheckout) (EveryLine cheap) `shouldBe` Right expected
      collapse expected `shouldBe` NotEligible

    it "handles singleton quantifiers" $ do
      let checkout = exampleCheckout { Checkout.lines = [electronicsLine] }
      interpret (PurchaseContext checkout) (AnyLine cheap)
        `shouldBe` Right (EvaluatedAnyLine [(electronicsLineId, priceLeaf expensivePrice NotEligible)])
      interpret (PurchaseContext checkout) (EveryLine cheap)
        `shouldBe` Right (EvaluatedEveryLine [(electronicsLineId, priceLeaf expensivePrice NotEligible)])

    it "keeps different line IDs for repeated products" $ do
      let secondBook = bookLine { Checkout.lineId = Checkout.LineId "second-book", Checkout.currentPrice = expensivePrice }
          checkout = exampleCheckout { Checkout.lines = [bookLine, secondBook] }
      interpret (PurchaseContext checkout) (AnyLine cheap)
        `shouldBe` Right (EvaluatedAnyLine
          [(bookLineId, priceLeaf cheapPrice Eligible), (Checkout.LineId "second-book", priceLeaf expensivePrice NotEligible)])

    it "retains nested quantified subtrees and restores the enclosing line context" $ do
      let checkout = exampleCheckout { Checkout.lines = [bookLine, electronicsLine] }
          inner = EvaluatedEveryLine
            [(bookLineId, currencyLeaf), (electronicsLineId, currencyLeaf)]
      interpret (PurchaseContext checkout) (AnyLine (And (EveryLine usd) cheap))
        `shouldBe` Right (EvaluatedAnyLine
          [ (bookLineId, EvaluatedAnd inner (priceLeaf cheapPrice Eligible))
          , (electronicsLineId, EvaluatedAnd inner (priceLeaf expensivePrice NotEligible))
          ])

    it "propagates an invalid line context as an error, even under Not" $
      interpret (PurchaseContext exampleCheckout) (Not cheap)
        `shouldBe` Left (MissingLine (LineRule (PriceLessThan priceThreshold)))

    it "does not turn unsupported rules into ordinary misses" $
      interpret (PurchaseContext exampleCheckout) (purchase (ShippingProviderIs Checkout.USPS))
        `shouldBe` Left (UnsupportedRule (PurchaseRule (ShippingProviderIs Checkout.USPS)))

    it "preserves primitive effects in tree order without Boolean short-circuiting" $ do
      let logging :: Interpreter ((,) [Bool]) Context Bool ((), Eligibility)
          logging = Interpreter $ \_ rule -> ([rule], ((), fromBool rule))
      evaluate (interpretEligibilityExpr logging) (PurchaseContext exampleCheckout)
        (Or (Check True) (And (Check False) (Not (Check True))))
        `shouldBe`
          ( [True, False, True]
          , EvaluatedOr (Checked () True Eligible)
              (EvaluatedAnd (Checked () False NotEligible) (EvaluatedNot (Checked () True Eligible)))
          )

    it "visits all quantified instances in checkout order" $ do
      let logging :: Interpreter ((,) [Context]) Context Bool ((), Eligibility)
          logging = Interpreter $ \context rule -> ([context], ((), fromBool rule))
          (contexts, result) = evaluate (interpretEligibilityExpr logging)
            (PurchaseContext exampleCheckout) (AnyLine (Check True))
      contexts `shouldBe`
        [ LineContext exampleCheckout bookLine
        , LineContext exampleCheckout electronicsLine
        , LineContext exampleCheckout clothingLine
        ]
      collapse result `shouldBe` Eligible

  describe "EligibilityExpr structure" $ do
    let expression = And (AnyLine cheap) (Not (EveryLine (Or cheap usd)))
    it "round-trips through its base functor" $
      embed (project expression) `shouldBe` expression
    it "traverses primitives inside quantifiers" $
      traverse (Just . show) expression `shouldBe` Just (fmap show expression)

  describe "Evaluation collapse" $ do
    let hit = Checked () True Eligible
        miss = Checked () False NotEligible
    it "preserves leaf verdicts" $ do
      collapse hit `shouldBe` Eligible
      collapse miss `shouldBe` NotEligible
    it "handles Boolean truth tables" $
      map (\(left, right) -> (collapse (EvaluatedAnd left right), collapse (EvaluatedOr left right)))
        [(hit, hit), (hit, miss), (miss, hit), (miss, miss)]
        `shouldBe` [(Eligible, Eligible), (NotEligible, Eligible), (NotEligible, Eligible), (NotEligible, NotEligible)]
    it "negates both verdicts" $ do
      collapse (EvaluatedNot hit) `shouldBe` NotEligible
      collapse (EvaluatedNot miss) `shouldBe` Eligible
    it "rejects AnyLine when all lines miss" $
      collapse (EvaluatedAnyLine [(bookLineId, miss), (electronicsLineId, miss)]) `shouldBe` NotEligible
    it "accepts EveryLine when all lines hit" $
      collapse (EvaluatedEveryLine [(bookLineId, hit), (electronicsLineId, hit)]) `shouldBe` Eligible
    it "negates an aggregate without discarding its evidence" $ do
      let result = EvaluatedNot (EvaluatedAnyLine [(bookLineId, hit), (electronicsLineId, miss)])
      collapse result `shouldBe` NotEligible
      embed (project result) `shouldBe` result
    it "can derive the verdict by mapping interpreter output" $
      runIdentity (evaluate
        (fmap collapse (interpretEligibilityExpr (Interpreter $ \_ rule -> pure ((), fromBool rule))))
        (PurchaseContext exampleCheckout) (Not (Check False)))
        `shouldBe` Eligible
