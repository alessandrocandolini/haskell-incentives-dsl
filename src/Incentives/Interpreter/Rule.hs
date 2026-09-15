{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Incentives.Interpreter.Rule where

import Data.Functor.Identity (Identity (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Numeric.Natural (Natural)
import Incentives.Ast (Ast (..))
import qualified Incentives.CheckoutSummary as Checkout
import Incentives.CheckoutSummary (CheckoutSummary, Currency, Days, LineId, Price, ShippingProvider)
import Incentives.Eligibility (Eligibility)
import Incentives.Interpreter (Interpreter (..), contramapContext)
import Incentives.Interpreter.Ast (interpretAst)
import Incentives.Interpreter.Lines (interpretLines)
import Incentives.Rule

-- Each field is an independently supplied interpreter. These dispatchers only
-- select a field; predicates and data supply belong to the supplied leaves.
data LineInterpreters m context result = LineInterpreters
  { price :: Interpreter m context Price result
  , category :: Interpreter m context Text result
  , sellerAge :: Interpreter m context Days result
  }

interpretLineRule :: LineInterpreters m context result -> Interpreter m context LineRule result
interpretLineRule interpreters = Interpreter $ \context rule -> case rule of
  PriceLessThan threshold -> evaluate (price interpreters) context threshold
  ProductCategoryIs expected -> evaluate (category interpreters) context expected
  SellerAccountAgeGreaterThan threshold -> evaluate (sellerAge interpreters) context threshold

data PurchaseInterpreters m context result = PurchaseInterpreters
  { currency :: Interpreter m context Currency result
  , shippingProvider :: Interpreter m context ShippingProvider result
  , buyerAge :: Interpreter m context Days result
  , distinctSellerCount :: Interpreter m context Natural result
  }

interpretPurchaseRule :: PurchaseInterpreters m context result -> Interpreter m context PurchaseRule result
interpretPurchaseRule interpreters = Interpreter $ \context rule -> case rule of
  CurrencyIs expected -> evaluate (currency interpreters) context expected
  ShippingProviderIs expected -> evaluate (shippingProvider interpreters) context expected
  BuyerAccountAgeGreaterThan threshold -> evaluate (buyerAge interpreters) context threshold
  DistinctSellerCountGreaterThan threshold -> evaluate (distinctSellerCount interpreters) context threshold

-- A line rule cannot be interpreted without a current line.
data Context target where
  PurchaseContext :: CheckoutSummary -> Context 'Purchase
  LineContext :: CheckoutSummary -> Checkout.Line -> Context 'Line

checkoutSummary :: Context target -> CheckoutSummary
checkoutSummary (PurchaseContext checkout) = checkout
checkoutSummary (LineContext checkout _) = checkout

-- Quantifiers expand into And/Or trees of concrete rule results, retaining the
-- line that each primitive inspected. Purchase facts retain purchase scope.
data EvaluatedRule
  = EvaluatedLine LineId LineRule Eligibility
  | EvaluatedPurchase PurchaseRule Eligibility
  deriving (Eq, Show)

ruleEligibility :: EvaluatedRule -> Eligibility
ruleEligibility (EvaluatedLine _ _ verdict) = verdict
ruleEligibility (EvaluatedPurchase _ verdict) = verdict

interpretRule
  :: Applicative m
  => Interpreter m Checkout.Line LineRule Eligibility
  -> Interpreter m CheckoutSummary PurchaseRule Eligibility
  -> Interpreter m (Context target) (Rule target) (Ast EvaluatedRule)
interpretRule lineInterpreter purchaseInterpreter =
  fmap runIdentity <$> interpretRuleWith (Identity <$> lineInterpreter) (Identity <$> purchaseInterpreter)

-- Share scope dispatch and quantifier expansion between immediate and staged
-- results. Annotation maps through f without executing any pending effects.
interpretRuleWith
  :: (Applicative m, Functor f)
  => Interpreter m Checkout.Line LineRule (f Eligibility)
  -> Interpreter m CheckoutSummary PurchaseRule (f Eligibility)
  -> Interpreter m (Context target) (Rule target) (Ast (f EvaluatedRule))
interpretRuleWith lineInterpreter purchaseInterpreter = Interpreter $ \context rule ->
  let checkout = checkoutSummary context
      quantified combine child =
        let conditions = contramapContext (LineContext checkout)
              (interpretAst (interpretRuleWith lineInterpreter purchaseInterpreter))
            combineLines ((_, first) :| rest) = foldl combine first (map snd rest)
        in combineLines
             <$> evaluate (interpretLines conditions) (Checkout.lines checkout) child
  in case rule of
    LineRule primitive -> case context of
      LineContext _ checkoutLine ->
        Pure . fmap (EvaluatedLine (Checkout.lineId checkoutLine) primitive)
          <$> evaluate lineInterpreter checkoutLine primitive
    PurchaseRule primitive ->
      Pure . fmap (EvaluatedPurchase primitive) <$> evaluate purchaseInterpreter checkout primitive
    AnyLine child -> quantified Or child
    EveryLine child -> quantified And child
