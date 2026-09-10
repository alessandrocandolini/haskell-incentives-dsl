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
        runIdentity (ProductDetails.fetch stubProductDetailsClient identifier)
          `shouldBe` Just details) (Map.toList productDetailsById)
    it "returns Nothing for an unknown product" $
      runIdentity (ProductDetails.fetch stubProductDetailsClient (ProductId "missing"))
        `shouldBe` Nothing
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
    it "derives fetch from one singleton batch, preserving effects" $ do
      let client = ProductDetails.fromFetchBatch $ \identifiers ->
            ([identifiers], Map.restrictKeys productDetailsById (NESet.toSet identifiers))
      ProductDetails.fetch client bookId
        `shouldBe` ([NESet.singleton bookId], Just bookDetails)
      ProductDetails.fetch client (ProductId "missing")
        `shouldBe` ([NESet.singleton (ProductId "missing")], Nothing)

  describe "UserDetailsClient stub" $ do
    it "returns the buyer and both sellers by ID" $
      mapM_ (\(identifier, details) ->
        runIdentity (UserDetails.fetch stubUserDetailsClient identifier)
          `shouldBe` Just details) (Map.toList userDetailsById)
    it "returns Nothing for an unknown user" $
      runIdentity (UserDetails.fetch stubUserDetailsClient (UserId "missing"))
        `shouldBe` Nothing
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
    it "derives fetch from one singleton batch, preserving effects" $ do
      let client = UserDetails.fromFetchBatch $ \identifiers ->
            ([identifiers], Map.restrictKeys userDetailsById (NESet.toSet identifiers))
      UserDetails.fetch client exampleBuyerId
        `shouldBe` ([NESet.singleton exampleBuyerId], Just buyerDetails)
      UserDetails.fetch client (UserId "missing")
        `shouldBe` ([NESet.singleton (UserId "missing")], Nothing)
