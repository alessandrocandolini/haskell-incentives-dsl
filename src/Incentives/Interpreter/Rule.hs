{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Incentives.Interpreter.Rule where

import Data.Text (Text)
import Numeric.Natural (Natural)
import qualified Incentives.CheckoutSummary as Checkout
import Incentives.CheckoutSummary (CheckoutSummary, Currency, Days, Price, ShippingProvider)
import Incentives.Eligibility (Eligibility (..), fromBool)
import Incentives.Interpreter (Interpreter (..), contramapContext)
import Incentives.Interpreter.Ast (interpretAst)
import Incentives.Interpreter.Lines (interpretLines)
import Incentives.Rule

-- Each field is an independently supplied interpreter. These dispatchers only
-- select a field; predicates and data supply belong to the supplied leaves.
data LineInterpreters m context = LineInterpreters
  { price :: Interpreter m context Price Eligibility
  , category :: Interpreter m context Text Eligibility
  , sellerAge :: Interpreter m context Days Eligibility
  }

interpretLineRule :: LineInterpreters m context -> Interpreter m context LineRule Eligibility
interpretLineRule interpreters = Interpreter $ \context rule -> case rule of
  PriceLessThan threshold -> evaluate (price interpreters) context threshold
  ProductCategoryIs expected -> evaluate (category interpreters) context expected
  SellerAccountAgeGreaterThan threshold -> evaluate (sellerAge interpreters) context threshold

data PurchaseInterpreters m context = PurchaseInterpreters
  { currency :: Interpreter m context Currency Eligibility
  , shippingProvider :: Interpreter m context ShippingProvider Eligibility
  , buyerAge :: Interpreter m context Days Eligibility
  , distinctSellerCount :: Interpreter m context Natural Eligibility
  }

interpretPurchaseRule :: PurchaseInterpreters m context -> Interpreter m context PurchaseRule Eligibility
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

interpretRule
  :: Applicative m
  => Interpreter m Checkout.Line LineRule Eligibility
  -> Interpreter m CheckoutSummary PurchaseRule Eligibility
  -> Interpreter m (Context target) (Rule target) Eligibility
interpretRule lineInterpreter purchaseInterpreter = Interpreter $ \context rule ->
  let checkout = checkoutSummary context
      quantified combine child =
        let conditions = contramapContext (LineContext checkout)
              (interpretAst (interpretRule lineInterpreter purchaseInterpreter))
        in fromBool . combine ((== Eligible) . snd)
             <$> evaluate (interpretLines conditions) (Checkout.lines checkout) child
  in case rule of
    LineRule primitive -> case context of
      LineContext _ checkoutLine -> evaluate lineInterpreter checkoutLine primitive
    PurchaseRule primitive -> evaluate purchaseInterpreter checkout primitive
    AnyLine child -> quantified any child
    EveryLine child -> quantified all child
