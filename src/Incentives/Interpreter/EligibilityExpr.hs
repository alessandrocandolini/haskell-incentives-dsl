module Incentives.Interpreter.EligibilityExpr where

import Data.Functor.Foldable (cata)
import qualified Incentives.CheckoutSummary as Checkout
import Incentives.CheckoutSummary (CheckoutSummary, Line)
import Incentives.Eligibility (Eligibility)
import Incentives.EligibilityExpr (EligibilityExpr, EligibilityExprF (..))
import Incentives.Evaluation (Evaluation (..))
import Incentives.Interpreter (Interpreter (..))

data Context
  = PurchaseContext CheckoutSummary
  | LineContext CheckoutSummary Line
  deriving (Eq, Show)

checkoutOf :: Context -> CheckoutSummary
checkoutOf (PurchaseContext checkout) = checkout
checkoutOf (LineContext checkout _) = checkout

-- Successful evaluation retains all branches, without Boolean short-circuiting.
-- The supplied primitive interpreter handles invalid contexts and missing data.
interpretEligibilityExpr
  :: Applicative m
  => Interpreter m Context rule (observed, Eligibility)
  -> Interpreter m Context (EligibilityExpr rule) (Evaluation observed rule)
interpretEligibilityExpr primitive = Interpreter $ \context expression ->
  cata algebra expression context
  where
    algebra (CheckF rule) context =
      (\(observed, verdict) -> Checked observed rule verdict)
        <$> evaluate primitive context rule
    algebra (AndF left right) context = EvaluatedAnd <$> left context <*> right context
    algebra (OrF left right) context = EvaluatedOr <$> left context <*> right context
    algebra (NotF child) context = EvaluatedNot <$> child context
    algebra (AnyLineF child) context = EvaluatedAnyLine <$> perLine child context
    algebra (EveryLineF child) context = EvaluatedEveryLine <$> perLine child context

    -- Quantifiers range over the full checkout, including when nested.
    perLine child context =
      let checkout = checkoutOf context
      in traverse
        (\checkoutLine ->
          (,) (Checkout.lineId checkoutLine) <$> child (LineContext checkout checkoutLine))
        (Checkout.lines checkout)
