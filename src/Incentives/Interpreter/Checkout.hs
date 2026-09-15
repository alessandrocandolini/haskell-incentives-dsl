module Incentives.Interpreter.Checkout
  ( fullCheckoutInterpreter
  , twoStageCheckoutInterpreter
  , partialCheckoutRuleInterpreter
  , checkoutLineInterpreters
  , checkoutPurchaseInterpreters
  ) where

import Data.Functor.Contravariant (contramap)
import Data.Functor.Identity (Identity)
import Data.Text (Text)
import Data.Time (UTCTime, diffUTCTime)
import Incentives.Ast (Ast)
import Incentives.Campaign (Campaign)
import qualified Incentives.CheckoutSummary as Checkout
import Incentives.CheckoutSummary (CheckoutSummary, Days, Line, ProductId, UserId, days)
import qualified Incentives.Client.ProductDetails as Products
import qualified Incentives.Client.UserDetails as Users
import Incentives.Eligibility (Eligibility)
import Incentives.Interpreter (Interpreter (..), pack, unpack, contramapContextM)
import Incentives.Interpreter.Ast (interpretAst, minimumWitness)
import Incentives.Interpreter.BuyerAccountAgeGreaterThan (buyerAccountAgeGreaterThan)
import Incentives.Interpreter.Campaign (CampaignResult, interpretCampaigns)
import Incentives.Interpreter.CurrencyIs (currencyIs)
import Incentives.Interpreter.DistinctSellerCountGreaterThan (distinctSellerCountGreaterThan)
import Incentives.Interpreter.PriceLessThan (priceLessThan)
import Incentives.Interpreter.ProductCategoryIs (productCategoryIs)
import Incentives.Interpreter.Rule
  ( Context (..), EvaluatedRule, PendingRule (..), LineInterpreters (..), PurchaseInterpreters (..)
  , checkoutSummary, interpretLineRule, interpretPurchaseRule, interpretRule, interpretRulePartially, ruleEligibility
  )
import Incentives.Interpreter.SellerAccountAgeGreaterThan (sellerAccountAgeGreaterThan)
import Incentives.Interpreter.ShippingProviderIs (shippingProviderIs)
import Incentives.Interpreter.TwoStage (interpretAstTwoStage)
import Incentives.Rule (Rule (..), LineRule (..), PurchaseRule (..))

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
  interpretCampaigns (interpretAstTwoStage ruleEligibility partialCheckoutRuleInterpreter secondPass)
  where
    fullRules = interpretRule
      (interpretLineRule (checkoutLineInterpreters products users now))
      (interpretPurchaseRule (checkoutPurchaseInterpreters users now))
    secondPass = Interpreter $ \context pending ->
      let checkout = checkoutSummary context
      in case pending of
        PendingLine checkoutLine primitive ->
          evaluate fullRules (LineContext checkout checkoutLine) (LineRule primitive)
        PendingPurchase primitive ->
          evaluate fullRules (PurchaseContext checkout) (PurchaseRule primitive)

partialCheckoutRuleInterpreter
  :: Interpreter Identity (Context target) (Rule target) (Ast (Either EvaluatedRule PendingRule))
partialCheckoutRuleInterpreter = interpretRulePartially
  (interpretLineRule partialLines)
  (interpretPurchaseRule partialPurchase)
  where
    partialLines = LineInterpreters
      { price = Left <$> (unpack $ contramap Checkout.currentPrice $ pack priceLessThan)
      , category = Interpreter $ \_ expected -> pure (Right (ProductCategoryIs expected))
      , sellerAge = Interpreter $ \_ threshold -> pure (Right (SellerAccountAgeGreaterThan threshold))
      }
    partialPurchase = PurchaseInterpreters
      { currency = Left <$> (unpack $ contramap Checkout.currency $ pack currencyIs)
      , shippingProvider = Left <$> (unpack $ contramap (Checkout.shippingProvider . Checkout.shipping) $ pack shippingProviderIs)
      , buyerAge = Interpreter $ \_ threshold -> pure (Right (BuyerAccountAgeGreaterThan threshold))
      , distinctSellerCount = Left <$> (unpack $ contramap (fmap Checkout.sellerId . Checkout.lines) $ pack distinctSellerCountGreaterThan)
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
  { price = unpack $ contramap Checkout.currentPrice $ pack priceLessThan
  , category = contramapContextM (fetchCategory products . Checkout.productId) productCategoryIs
  , sellerAge = contramapContextM (fetchAccountAge users now . Checkout.sellerId) sellerAccountAgeGreaterThan
  }

checkoutPurchaseInterpreters
  :: MonadFail m
  => Users.UserDetailsClient m
  -> UTCTime
  -> PurchaseInterpreters m CheckoutSummary Eligibility
checkoutPurchaseInterpreters users now = PurchaseInterpreters
  { currency = unpack $ contramap Checkout.currency $ pack currencyIs
  , shippingProvider = unpack $ contramap (Checkout.shippingProvider . Checkout.shipping) $ pack shippingProviderIs
  , buyerAge = contramapContextM (fetchAccountAge users now . Checkout.buyerId) buyerAccountAgeGreaterThan
  , distinctSellerCount = unpack $ contramap (fmap Checkout.sellerId . Checkout.lines) $ pack distinctSellerCountGreaterThan
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
