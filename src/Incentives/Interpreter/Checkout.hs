module Incentives.Interpreter.Checkout
  ( fullCheckoutInterpreter
  , twoStageCheckoutInterpreter
  , fetchLineInterpreters
  , fetchPurchaseInterpreters
  , partialCheckoutRuleInterpreter
  , checkoutLineInterpreters
  , checkoutPurchaseInterpreters
  ) where

import Control.Selective (Selective)
import qualified Data.Map.Strict as Map
import qualified Data.Set.NonEmpty as NESet
import Data.Functor.Contravariant (contramap)
import Data.Functor.Identity (Identity, runIdentity)
import Data.Text (Text)
import Data.Time (UTCTime, diffUTCTime)
import Incentives.Ast (Ast)
import Incentives.Campaign (Campaign)
import qualified Incentives.CheckoutSummary as Checkout
import Incentives.CheckoutSummary (CheckoutSummary, Days, Line, ProductId, UserId, days)
import qualified Incentives.Client.ProductDetails as Products
import qualified Incentives.Client.UserDetails as Users
import Incentives.Eligibility (Eligibility)
import Incentives.Fetch (Fetch, Request, fetchProduct, fetchUser)
import Incentives.Interpreter (Interpreter (..), pack, unpack)
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
  :: (MonadFail m, Selective m)
  => Products.ProductDetailsClient m
  -> Users.UserDetailsClient m
  -> UTCTime
  -> Interpreter m CheckoutSummary [Campaign] [CampaignResult]
fullCheckoutInterpreter products users now =
  interpretCampaigns (minimumWitness ruleEligibility <$> interpretAst (interpretRule
    (interpretLineRule (checkoutLineInterpreters products users now))
    (interpretPurchaseRule (checkoutPurchaseInterpreters users now))))

twoStageCheckoutInterpreter
  :: UTCTime
  -> Interpreter (Fetch Request) CheckoutSummary [Campaign] [CampaignResult]
twoStageCheckoutInterpreter now =
  interpretCampaigns (interpretAstTwoStage ruleEligibility partialCheckoutRuleInterpreter secondPass)
  where
    fullRules = interpretRule
      (interpretLineRule (fetchLineInterpreters now))
      (interpretPurchaseRule (fetchPurchaseInterpreters now))
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

checkoutLineInterpreters
  :: MonadFail m
  => Products.ProductDetailsClient m
  -> Users.UserDetailsClient m
  -> UTCTime
  -> LineInterpreters m Line Eligibility
checkoutLineInterpreters products users now =
  lineInterpretersWith (fetchCategory products) (fetchAccountAge users now)

checkoutPurchaseInterpreters
  :: MonadFail m
  => Users.UserDetailsClient m
  -> UTCTime
  -> PurchaseInterpreters m CheckoutSummary Eligibility
checkoutPurchaseInterpreters users now =
  purchaseInterpretersWith (fetchAccountAge users now)

fetchLineInterpreters :: UTCTime -> LineInterpreters (Fetch Request) Line Eligibility
fetchLineInterpreters now = lineInterpretersWith
  (fmap Products.category . fetchProduct)
  (fmap (accountAge now) . fetchUser)

fetchPurchaseInterpreters :: UTCTime -> PurchaseInterpreters (Fetch Request) CheckoutSummary Eligibility
fetchPurchaseInterpreters now = purchaseInterpretersWith (fmap (accountAge now) . fetchUser)

lineInterpretersWith
  :: Applicative m
  => (ProductId -> m Text)
  -> (UserId -> m Days)
  -> LineInterpreters m Line Eligibility
lineInterpretersWith categoryFor ageFor = LineInterpreters
  { price = unpack $ contramap Checkout.currentPrice $ pack priceLessThan
  , category = supply (categoryFor . Checkout.productId) productCategoryIs
  , sellerAge = supply (ageFor . Checkout.sellerId) sellerAccountAgeGreaterThan
  }

purchaseInterpretersWith
  :: Applicative m
  => (UserId -> m Days)
  -> PurchaseInterpreters m CheckoutSummary Eligibility
purchaseInterpretersWith ageFor = PurchaseInterpreters
  { currency = unpack $ contramap Checkout.currency $ pack currencyIs
  , shippingProvider = unpack $ contramap (Checkout.shippingProvider . Checkout.shipping) $ pack shippingProviderIs
  , buyerAge = supply (ageFor . Checkout.buyerId) buyerAccountAgeGreaterThan
  , distinctSellerCount = unpack $ contramap (fmap Checkout.sellerId . Checkout.lines) $ pack distinctSellerCountGreaterThan
  }

supply
  :: Functor m
  => (outer -> m inner)
  -> Interpreter Identity inner input output
  -> Interpreter m outer input output
supply contextFor interpreter = Interpreter $ \context input ->
  (\inner -> runIdentity (evaluate interpreter inner input)) <$> contextFor context

fetchCategory :: MonadFail m => Products.ProductDetailsClient m -> ProductId -> m Text
fetchCategory products ident = do
  result <- Products.fetchBatch products (NESet.singleton ident)
  maybe (fail ("Missing product details: " ++ show ident)) (pure . Products.category) (Map.lookup ident result)

fetchAccountAge :: MonadFail m => Users.UserDetailsClient m -> UTCTime -> UserId -> m Days
fetchAccountAge users now ident = do
  result <- Users.fetchBatch users (NESet.singleton ident)
  maybe (fail ("Missing user details: " ++ show ident)) (pure . accountAge now) (Map.lookup ident result)

accountAge :: UTCTime -> Users.UserDetails -> Days
accountAge now details =
  let elapsedDays = floor (diffUTCTime now (Users.accountCreatedAt details) / 86400)
  in days (fromInteger (max 0 elapsedDays))
