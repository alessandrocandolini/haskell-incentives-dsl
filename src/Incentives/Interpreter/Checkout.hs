module Incentives.Interpreter.Checkout
  ( fullCheckoutInterpreter
  , checkoutLineInterpreters
  , checkoutPurchaseInterpreters
  ) where

import Data.Text (Text)
import Data.Time (UTCTime, diffUTCTime)
import Incentives.Campaign (Campaign, CampaignId)
import qualified Incentives.CheckoutSummary as Checkout
import Incentives.CheckoutSummary (CheckoutSummary, Days, Line, ProductId, UserId, days)
import qualified Incentives.Client.ProductDetails as Products
import qualified Incentives.Client.UserDetails as Users
import Incentives.GrantedIncentive (GrantedIncentive)
import Incentives.Interpreter (Interpreter, contramapContext, contramapContextM)
import Incentives.Interpreter.Ast (interpretAst)
import Incentives.Interpreter.BuyerAccountAgeGreaterThan (buyerAccountAgeGreaterThan)
import Incentives.Interpreter.Campaign (interpretCampaigns)
import Incentives.Interpreter.CurrencyIs (currencyIs)
import Incentives.Interpreter.DistinctSellerCountGreaterThan (distinctSellerCountGreaterThan)
import Incentives.Interpreter.PriceLessThan (priceLessThan)
import Incentives.Interpreter.ProductCategoryIs (productCategoryIs)
import Incentives.Interpreter.Rule
  ( LineInterpreters (..), PurchaseInterpreters (..)
  , interpretLineRule, interpretPurchaseRule, interpretRule
  )
import Incentives.Interpreter.SellerAccountAgeGreaterThan (sellerAccountAgeGreaterThan)
import Incentives.Interpreter.ShippingProviderIs (shippingProviderIs)

-- Compose the full, unoptimised interpreter from independently wired leaves.
fullCheckoutInterpreter
  :: MonadFail m
  => Products.ProductDetailsClient m
  -> Users.UserDetailsClient m
  -> UTCTime
  -> Interpreter m CheckoutSummary [Campaign] [(CampaignId, GrantedIncentive)]
fullCheckoutInterpreter products users now =
  interpretCampaigns (interpretAst (interpretRule
    (interpretLineRule (checkoutLineInterpreters products users now))
    (interpretPurchaseRule (checkoutPurchaseInterpreters users now))))

-- Checkout-specific supply for the existing, narrow rule interpreters.
-- This baseline uses individual lookups; batching and pruning are still absent.
checkoutLineInterpreters
  :: MonadFail m
  => Products.ProductDetailsClient m
  -> Users.UserDetailsClient m
  -> UTCTime
  -> LineInterpreters m Line
checkoutLineInterpreters products users now = LineInterpreters
  { price = contramapContext Checkout.currentPrice priceLessThan
  , category = contramapContextM (fetchCategory products . Checkout.productId) productCategoryIs
  , sellerAge = contramapContextM (fetchAccountAge users now . Checkout.sellerId) sellerAccountAgeGreaterThan
  }

checkoutPurchaseInterpreters
  :: MonadFail m
  => Users.UserDetailsClient m
  -> UTCTime
  -> PurchaseInterpreters m CheckoutSummary
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
