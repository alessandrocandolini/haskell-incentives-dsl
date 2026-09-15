module Incentives.ClientSpec where

import Data.Functor.Identity (runIdentity)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import qualified Data.Set.NonEmpty as NESet
import Incentives.CheckoutSummary (ProductId (..), UserId (..))
import qualified Incentives.Client.ProductDetails as ProductDetails
import Incentives.Client.Stub
import qualified Incentives.Client.UserDetails as UserDetails
import Incentives.ExampleData
import Test.Hspec

spec :: Spec
spec = do
  describe "ProductDetailsClient stub" $ do
    it "returns every configured product by ID" $
      mapM_ (\(identifier, details) ->
        runIdentity (ProductDetails.fetchBatch stubProductDetailsClient (NESet.singleton identifier))
          `shouldBe` Map.singleton identifier details) (Map.toList productDetailsById)
    it "omits an unknown product from a singleton batch" $
      runIdentity (ProductDetails.fetchBatch stubProductDetailsClient (NESet.singleton (ProductId "missing")))
        `shouldBe` Map.empty
    it "returns only requested known products in a mixed batch" $
      runIdentity (ProductDetails.fetchBatch stubProductDetailsClient
        (NESet.fromList (bookId :| [ProductId "missing", clothingId])))
        `shouldBe` Map.fromList [(bookId, bookDetails), (clothingId, clothingDetails)]
    it "returns an empty map if no requested products exist" $
      runIdentity (ProductDetails.fetchBatch stubProductDetailsClient
        (NESet.singleton (ProductId "missing")))
        `shouldBe` Map.empty
    it "deduplicates repeated IDs through the non-empty set" $
      runIdentity (ProductDetails.fetchBatch stubProductDetailsClient
        (NESet.fromList (bookId :| [bookId])))
        `shouldBe` Map.singleton bookId bookDetails

  describe "UserDetailsClient stub" $ do
    it "returns the buyer and both sellers by ID" $
      mapM_ (\(identifier, details) ->
        runIdentity (UserDetails.fetchBatch stubUserDetailsClient (NESet.singleton identifier))
          `shouldBe` Map.singleton identifier details) (Map.toList userDetailsById)
    it "omits an unknown user from a singleton batch" $
      runIdentity (UserDetails.fetchBatch stubUserDetailsClient (NESet.singleton (UserId "missing")))
        `shouldBe` Map.empty
    it "returns only requested known users in a mixed batch" $
      runIdentity (UserDetails.fetchBatch stubUserDetailsClient
        (NESet.fromList (exampleBuyerId :| [UserId "missing", recentSellerId])))
        `shouldBe` Map.fromList
          [(exampleBuyerId, buyerDetails), (recentSellerId, recentSellerDetails)]
    it "returns an empty map if no requested users exist" $
      runIdentity (UserDetails.fetchBatch stubUserDetailsClient
        (NESet.singleton (UserId "missing")))
        `shouldBe` Map.empty
    it "deduplicates repeated IDs through the non-empty set" $
      runIdentity (UserDetails.fetchBatch stubUserDetailsClient
        (NESet.fromList (exampleBuyerId :| [exampleBuyerId])))
        `shouldBe` Map.singleton exampleBuyerId buyerDetails
