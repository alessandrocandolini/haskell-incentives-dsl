module Incentives.EligibilitySpec where

import Control.Monad (forM_)
import Incentives.Ast (Ast (..))
import Incentives.Eligibility
import Test.Hspec

spec :: Spec
spec = describe "evaluate" $ do
  forM_ [minBound .. maxBound] $ \result ->
    it ("preserves a " ++ show result ++ " leaf") $
      evaluate (Pure result) `shouldBe` result

  forM_
    [ (Eligible, Eligible, Eligible, Eligible)
    , (Eligible, NotEligible, NotEligible, Eligible)
    , (NotEligible, Eligible, NotEligible, Eligible)
    , (NotEligible, NotEligible, NotEligible, NotEligible)
    ] $ \(left, right, conjunction, disjunction) -> do
      it ("evaluates And " ++ show (left, right)) $
        evaluate (And (Pure left) (Pure right)) `shouldBe` conjunction
      it ("evaluates Or " ++ show (left, right)) $
        evaluate (Or (Pure left) (Pure right)) `shouldBe` disjunction

  it "negates Eligible" $
    evaluate (Not (Pure Eligible)) `shouldBe` NotEligible
  it "negates NotEligible" $
    evaluate (Not (Pure NotEligible)) `shouldBe` Eligible
  it "folds nested Boolean expressions" $
    evaluate
      (Not (And (Or (Pure NotEligible) (Pure Eligible)) (Not (Pure NotEligible))))
      `shouldBe` NotEligible
