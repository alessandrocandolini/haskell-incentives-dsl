{-# LANGUAGE DataKinds #-}

module Incentives.OptimisationSpec where

import Control.Monad (unless)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Set.NonEmpty as NESet
import Data.Time (addUTCTime)
import Incentives.Ast
import Incentives.Campaign
import qualified Incentives.CheckoutSummary as Checkout
import Incentives.CheckoutSummary (Currency (..), LineId (..), ProductId (..), UserId (..), amount, days)
import qualified Incentives.Client.ProductDetails as Products
import qualified Incentives.Client.UserDetails as Users
import Incentives.ExampleData (exampleNow, campaignStartsAt, campaignEndsAt)
import Incentives.Eligibility (Eligibility (..))
import Incentives.GrantedIncentive
import Incentives.Rule
import Incentives.Interpreter (evaluate)
import Incentives.Interpreter.Campaign (CampaignResult (..))
import Incentives.Interpreter.Checkout (twoStageCheckoutInterpreter)
import Incentives.Interpreter.Rule (EvaluatedRule (..))
import Test.Hspec

-- Pruning has its own acceptance tests. The shared-batching test remains RED
-- until surviving requests can be collected across campaigns and checkout lines.
spec :: Spec
spec = do
  describe "Pruning without batching" pruningSpec
  describe "Pruning and shared batching" batchingSpec

batchingSpec :: Spec
batchingSpec =
  it "prunes and batches across two campaigns, five lines, three sellers and one buyer" $ do
    -- First campaign needs products 1/2/3; second needs 2/4 after the pure pass.
    -- Product 5 and seller 3 must never be fetched. Buyer and seller facts share
    -- one user batch across lines AND campaigns; product 2 is also shared.
    (products, users, assertRequests) <- strictClients
      (Set.fromList [product1, product2, product3, product4])
      (Set.fromList [buyer, seller1, seller2])
    result <- evaluate (twoStageCheckoutInterpreter products users exampleNow) checkout [bookCampaign, boostingCampaign]
    attributedResults result `shouldMatchList`
      [ (campaignId bookCampaign, LineGrant line1 (Waive SellingFee))
      , (campaignId boostingCampaign, LineGrant line1 (Waive BoostingFee))
      , (campaignId boostingCampaign, LineGrant line3 (Waive BoostingFee))
      , (campaignId boostingCampaign, LineGrant line4 (Waive BoostingFee))
      ]
    assertRequests

pruningSpec :: Spec
pruningSpec = do
  it "makes no upstream requests when the pure currency gate rejects both campaigns" $ do
    (products, users, assertNoRequests) <- unavailableClients
    result <- evaluate (twoStageCheckoutInterpreter products users exampleNow) (inexpensiveCheckout { Checkout.currency = GBP })
      [bookCampaign, boostingCampaign]
    map grantedIncentives result `shouldBe` [[], []]
    map evaluatedConditions result `shouldBe` replicate 2
      [ (Just ident, Pure (EvaluatedPurchase (CurrencyIs USD) NotEligible))
      | ident <- [line1, line2, line3, line4, line5]
      ]
    assertNoRequests

  it "grants on a true pure Or branch without fetching its effectful sibling" $ do
    (products, users, assertNoRequests) <- unavailableClients
    result <- evaluate (twoStageCheckoutInterpreter products users exampleNow) inexpensiveCheckout [boostingCampaign]
    attributedResults result `shouldMatchList`
      [ (campaignId boostingCampaign, LineGrant ident (Waive BoostingFee))
      | ident <- [line1, line2, line3, line4, line5]
      ]
    map evaluatedConditions result `shouldBe`
      [ [ (Just ident, And
            (Pure (EvaluatedLine ident (PriceLessThan (amount 10)) Eligible))
            (Pure (EvaluatedPurchase (CurrencyIs USD) Eligible)))
        | ident <- [line1, line2, line3, line4, line5]
        ]
      ]
    assertNoRequests

  it "prunes effectful branches inside quantified conditions and preserves negation" $ do
    (products, users, assertNoRequests) <- unavailableClients
    let existential = bookCampaign
          { offering = when
              (anyLine (line (ProductCategoryIs "books") .||. line (PriceLessThan (amount 10))))
              (grant (Waive ShippingCost))
          }
        negatedUniversal = boostingCampaign
          { offering = when
              (Not (everyLine (line (ProductCategoryIs "books") .&&. line (PriceLessThan (amount 1)))))
              (grant (Waive ShippingCost))
          }
    result <- evaluate (twoStageCheckoutInterpreter products users exampleNow) inexpensiveCheckout
      [existential, negatedUniversal]
    result `shouldBe`
      [ CampaignResult (campaignId existential) [PurchaseGrant (Waive ShippingCost)]
          [(Nothing, Pure (EvaluatedLine line1 (PriceLessThan (amount 10)) Eligible))]
      , CampaignResult (campaignId negatedUniversal) [PurchaseGrant (Waive ShippingCost)]
          [(Nothing, Not (Pure (EvaluatedLine line1 (PriceLessThan (amount 1)) NotEligible)))]
      ]
    assertNoRequests

-- There is no successful client response here, even for a batch. Recording
-- before throwing also catches an interpreter that swallows a client failure.
unavailableClients
  :: IO (Products.ProductDetailsClient IO, Users.UserDetailsClient IO, Expectation)
unavailableClients = do
  requests <- newIORef []
  let unexpected request = do
        atomicModifyIORef' requests (\previous -> (request : previous, ()))
        fail ("Pure pruning must avoid every upstream request: " ++ show request)
      products = Products.ProductDetailsClient
        { Products.fetch = unexpected . ProductFetch
        , Products.fetchBatch = unexpected . ProductBatch . NESet.toSet
        }
      users = Users.UserDetailsClient
        { Users.fetch = unexpected . UserFetch
        , Users.fetchBatch = unexpected . UserBatch . NESet.toSet
        }
  pure (products, users, readIORef requests `shouldReturn` [])

inexpensiveCheckout :: Checkout.CheckoutSummary
inexpensiveCheckout = checkout
  { Checkout.lines = fmap (\checkoutLine -> checkoutLine { Checkout.currentPrice = amount 5 })
      (Checkout.lines checkout)
  }

attributedResults :: [CampaignResult] -> [(CampaignId, GrantedIncentive)]
attributedResults results =
  [ (resultCampaignId result, incentive)
  | result <- results
  , incentive <- grantedIncentives result
  ]

bookCampaign :: Campaign
bookCampaign = Campaign
  { campaignId = CampaignId "books-for-established-accounts"
  , startsAt = campaignStartsAt
  , endsAt = campaignEndsAt
  , offering = forEachLine $
      when
        ( ( line (ProductCategoryIs "books")
              .&&. line (SellerAccountAgeGreaterThan (days 365))
              .&&. purchase (BuyerAccountAgeGreaterThan (days 365))
          )
            .&&. line (PriceLessThan (amount 20))
            .&&. purchase (CurrencyIs USD)
        )
        (grant (Waive SellingFee))
  }

boostingCampaign :: Campaign
boostingCampaign = Campaign
  { campaignId = CampaignId "boost-inexpensive-lines-or-eligible-books"
  , startsAt = campaignStartsAt
  , endsAt = campaignEndsAt
  , offering = forEachLine $
      when
        ( ( ( line (ProductCategoryIs "books")
                  .&&. line (SellerAccountAgeGreaterThan (days 30))
                  .&&. purchase (BuyerAccountAgeGreaterThan (days 30))
                  .&&. line (PriceLessThan (amount 40))
              )
                .||. line (PriceLessThan (amount 10))
          )
            .&&. purchase (CurrencyIs USD)
        )
        (grant (Waive BoostingFee))
  }

-- Effectful checks deliberately precede the pure checks in the source above:
-- evaluating left-to-right with short-circuiting cannot satisfy these tests.
--
-- Line | Product | Seller | Price | Category
--   1  |    1    |    1   |    8  | books
--   2  |    2    |    1   |   18  | games
--   3  |    3    |    2   |    6  | clothing
--   4  |    4    |    2   |   30  | books
--   5  |    5    |    3   |  100  | books (never fetched)
checkout :: Checkout.CheckoutSummary
checkout = Checkout.CheckoutSummary
  { Checkout.currency = USD
  , Checkout.buyerId = buyer
  , Checkout.shipping = Checkout.ShippingSummary Checkout.USPS (amount 5)
  , Checkout.lines =
      makeLine line1 product1 seller1 8 :|
        [ makeLine line2 product2 seller1 18
        , makeLine line3 product3 seller2 6
        , makeLine line4 product4 seller2 30
        , makeLine line5 product5 seller3 100
        ]
  }
  where
    makeLine ident productIdent seller price = Checkout.Line ident productIdent seller (amount price) (amount price)

line1, line2, line3, line4, line5 :: LineId
line1 = LineId "line-1"
line2 = LineId "line-2"
line3 = LineId "line-3"
line4 = LineId "line-4"
line5 = LineId "line-5"

product1, product2, product3, product4, product5 :: ProductId
product1 = ProductId "product-1"
product2 = ProductId "product-2"
product3 = ProductId "product-3"
product4 = ProductId "product-4"
product5 = ProductId "product-5"

buyer, seller1, seller2, seller3 :: UserId
buyer = UserId "buyer"
seller1 = UserId "seller-1"
seller2 = UserId "seller-2"
seller3 = UserId "seller-3"

productFacts :: Map ProductId Products.ProductDetails
productFacts = Map.fromList
  [ (ident, Products.ProductDetails ident category)
  | (ident, category) <- [(product1, "books"), (product2, "games"), (product3, "clothing"), (product4, "books"), (product5, "books")]
  ]

userFacts :: Map UserId Users.UserDetails
userFacts = Map.fromList
  [ (ident, Users.UserDetails ident (addUTCTime (negate (age * 86400)) exampleNow))
  | (ident, age) <- [(buyer, 730), (seller1, 730), (seller2, 400), (seller3, 1)]
  ]

data Request
  = ProductFetch ProductId
  | ProductBatch (Set ProductId)
  | UserFetch UserId
  | UserBatch (Set UserId)
  deriving (Eq, Show)

-- A call is recorded before checking it, so even caught client exceptions leave
-- evidence. Batch order between sources is unrestricted; each source gets one
-- exact batch, or no call at all when its expected set is empty.
strictClients
  :: Set ProductId
  -> Set UserId
  -> IO (Products.ProductDetailsClient IO, Users.UserDetailsClient IO, Expectation)
strictClients expectedProducts expectedUsers = do
  requests <- newIORef []
  let record request = atomicModifyIORef' requests (\previous -> (request : previous, previous))
      products = Products.ProductDetailsClient
        { Products.fetch = \ident -> do
            _ <- record (ProductFetch ident)
            ioError (userError ("Forbidden products.fetch " ++ show ident ++ "; expected product batches: " ++ show expectedProducts))
        , Products.fetchBatch = \identifiers -> do
            let requested = NESet.toSet identifiers
            previous <- record (ProductBatch requested)
            unless (requested == expectedProducts) $
              ioError (userError ("products.fetchBatch: expected " ++ show expectedProducts ++ ", got " ++ show requested))
            unless (all (not . isProductRequest) previous) $
              ioError (userError "Products must be fetched in one shared batch across all campaigns")
            pure (Map.restrictKeys productFacts requested)
        }
      users = Users.UserDetailsClient
        { Users.fetch = \ident -> do
            _ <- record (UserFetch ident)
            ioError (userError ("Forbidden users.fetch " ++ show ident ++ "; expected user batches: " ++ show expectedUsers))
        , Users.fetchBatch = \identifiers -> do
            let requested = NESet.toSet identifiers
            previous <- record (UserBatch requested)
            unless (requested == expectedUsers) $
              ioError (userError ("users.fetchBatch: expected " ++ show expectedUsers ++ ", got " ++ show requested))
            unless (all (not . isUserRequest) previous) $
              ioError (userError "Buyer and sellers must be fetched in one shared batch across all campaigns")
            pure (Map.restrictKeys userFacts requested)
        }
      assertRequests = do
        actual <- readIORef requests
        actual `shouldMatchList`
          ([ProductBatch expectedProducts | not (Set.null expectedProducts)]
            ++ [UserBatch expectedUsers | not (Set.null expectedUsers)])
  pure (products, users, assertRequests)
  where
    isProductRequest (ProductFetch _) = True
    isProductRequest (ProductBatch _) = True
    isProductRequest _ = False
    isUserRequest (UserFetch _) = True
    isUserRequest (UserBatch _) = True
    isUserRequest _ = False
