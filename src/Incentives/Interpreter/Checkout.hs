module Incentives.Interpreter.Checkout
  ( fullCheckoutInterpreter
  , twoStageCheckoutInterpreter
  , checkoutLineInterpreters
  , checkoutPurchaseInterpreters
  ) where

import Data.Text (Text)
import Data.Time (UTCTime, diffUTCTime)
import Incentives.Campaign (Campaign)
import qualified Incentives.CheckoutSummary as Checkout
import Incentives.CheckoutSummary (CheckoutSummary, Days, Line, ProductId, UserId, days)
import qualified Incentives.Client.ProductDetails as Products
import qualified Incentives.Client.UserDetails as Users
import Incentives.Eligibility (Eligibility)
import Incentives.Interpreter (Interpreter, contramapContext, contramapContextM)
import Incentives.Interpreter.Ast (interpretAst, minimumWitness)
import Incentives.Interpreter.BuyerAccountAgeGreaterThan (buyerAccountAgeGreaterThan)
import Incentives.Interpreter.Campaign (CampaignResult, interpretCampaigns)
import Incentives.Interpreter.CurrencyIs (currencyIs)
import Incentives.Interpreter.DistinctSellerCountGreaterThan (distinctSellerCountGreaterThan)
import Incentives.Interpreter.PriceLessThan (priceLessThan)
import Incentives.Interpreter.ProductCategoryIs (productCategoryIs)
import Incentives.Interpreter.Rule
  ( LineInterpreters (..), PurchaseInterpreters (..)
  , interpretLineRule, interpretPurchaseRule, interpretRule, interpretRuleWith, ruleEligibility
  )
import Incentives.Interpreter.SellerAccountAgeGreaterThan (sellerAccountAgeGreaterThan)
import Incentives.Interpreter.ShippingProviderIs (shippingProviderIs)
import Incentives.Interpreter.TwoStage (known, deferred, interpretAstTwoStage)

-- Compose the full, unoptimised interpreter from independently wired leaves.
fullCheckoutInterpreter
  :: MonadFail m
  => Products.ProductDetailsClient m
  -> Users.UserDetailsClient m
  -> UTCTime
  -> Interpreter m CheckoutSummary [Campaign] [CampaignResult]
fullCheckoutInterpreter products users now =
  interpretCampaigns (minimumWitness ruleEligibility <$> interpretAst (interpretRule
    (interpretLineRule (checkoutLineInterpreters products users now))
    (interpretPurchaseRule (checkoutPurchaseInterpreters users now))))

-- The same campaign traversal, with cheap rules evaluated before client calls.
-- Surviving client calls still use individual lookups.
twoStageCheckoutInterpreter
  :: MonadFail m
  => Products.ProductDetailsClient m
  -> Users.UserDetailsClient m
  -> UTCTime
  -> Interpreter m CheckoutSummary [Campaign] [CampaignResult]
twoStageCheckoutInterpreter products users now =
  interpretCampaigns (interpretAstTwoStage ruleEligibility (interpretRuleWith
    (interpretLineRule stagedLines)
    (interpretPurchaseRule stagedPurchase)))
  where
    fullLines = checkoutLineInterpreters products users now
    fullPurchase = checkoutPurchaseInterpreters users now
    stagedLines = LineInterpreters
      { price = known (contramapContext Checkout.currentPrice priceLessThan)
      , category = deferred (category fullLines)
      , sellerAge = deferred (sellerAge fullLines)
      }
    stagedPurchase = PurchaseInterpreters
      { currency = known (contramapContext Checkout.currency currencyIs)
      , shippingProvider = known (contramapContext (Checkout.shippingProvider . Checkout.shipping) shippingProviderIs)
      , buyerAge = deferred (buyerAge fullPurchase)
      , distinctSellerCount = known (contramapContext (fmap Checkout.sellerId . Checkout.lines) distinctSellerCountGreaterThan)
      }

-- Checkout-specific supply for the existing, narrow rule interpreters.
-- This baseline uses individual lookups; batching and pruning are still absent.
checkoutLineInterpreters
  :: MonadFail m
  => Products.ProductDetailsClient m
  -> Users.UserDetailsClient m
  -> UTCTime
  -> LineInterpreters m Line Eligibility
checkoutLineInterpreters products users now = LineInterpreters
  { price = contramapContext Checkout.currentPrice priceLessThan
  , category = contramapContextM (fetchCategory products . Checkout.productId) productCategoryIs
  , sellerAge = contramapContextM (fetchAccountAge users now . Checkout.sellerId) sellerAccountAgeGreaterThan
  }

checkoutPurchaseInterpreters
  :: MonadFail m
  => Users.UserDetailsClient m
  -> UTCTime
  -> PurchaseInterpreters m CheckoutSummary Eligibility
checkoutPurchaseInterpreters users now = PurchaseInterpreters
  { currency = contramapContext Checkout.currency currencyIs
  , shippingProvider = contramapContext (Checkout.shippingProvider . Checkout.shipping) shippingProviderIs
  , buyerAge = contramapContextM (fetchAccountAge users now . Checkout.buyerId) buyerAccountAgeGreaterThan
  , distinctSellerCount = contramapContext (fmap Checkout.sellerId . Checkout.lines) distinctSellerCountGreaterThan
  }

fetchCategory :: MonadFail m => Products.ProductDetailsClient m -> ProductId -> m Text
fetchCategory products ident = do
  result <- Products.fetch products ident
  maybe (fail ("Missing product details: " ++ show ident)) (pure . Products.category) result

fetchAccountAge :: MonadFail m => Users.UserDetailsClient m -> UTCTime -> UserId -> m Days
fetchAccountAge users now ident = do
  result <- Users.fetch users ident
  details <- maybe (fail ("Missing user details: " ++ show ident)) pure result
  let elapsedDays = floor (diffUTCTime now (Users.accountCreatedAt details) / 86400)
  pure (days (fromInteger (max 0 elapsedDays)))
