{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Incentives.Fetch
  ( Fetch (..)
  , Request (..)
  , fetchProduct
  , fetchUser
  , runFetch
  ) where

import Control.Selective (Selective (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Set.NonEmpty (NESet)
import qualified Data.Set.NonEmpty as NESet
import Incentives.CheckoutSummary (ProductId, UserId)
import qualified Incentives.Client.ProductDetails as Products
import qualified Incentives.Client.UserDetails as Users
import UnliftIO (MonadUnliftIO, throwIO)
import UnliftIO.Async (concurrently)

data Request a where
  ProductRequest :: ProductId -> Request Products.ProductDetails
  UserRequest :: UserId -> Request Users.UserDetails

data Fetch request a where
  Pure :: a -> Fetch request a
  Request :: request a -> Fetch request a
  Apply :: Fetch request (a -> b) -> Fetch request a -> Fetch request b
  Select :: Fetch request (Either a b) -> Fetch request (a -> b) -> Fetch request b

instance Functor (Fetch request) where
  fmap f = Apply (Pure f)

instance Applicative (Fetch request) where
  pure = Pure
  (<*>) = Apply

instance Selective (Fetch request) where
  select = Select

fetchProduct :: ProductId -> Fetch Request Products.ProductDetails
fetchProduct = Request . ProductRequest

fetchUser :: UserId -> Fetch Request Users.UserDetails
fetchUser = Request . UserRequest

data Progress demand request a
  = Done a
  | Blocked demand (Fetch request a)

advance
  :: Semigroup demand
  => (forall value. request value -> Either demand value)
  -> Fetch request a
  -> Progress demand request a
advance _ (Pure value) = Done value
advance resolve (Request request) = case resolve request of
  Left demand -> Blocked demand (Request request)
  Right value -> Done value
advance resolve (Apply functions arguments) =
  case (advance resolve functions, advance resolve arguments) of
    (Done f, Done a) -> Done (f a)
    (Done f, Blocked demand rest) -> Blocked demand (Apply (Pure f) rest)
    (Blocked demand rest, Done a) -> Blocked demand (Apply rest (Pure a))
    (Blocked left fs, Blocked right xs) -> Blocked (left <> right) (Apply fs xs)
advance resolve (Select selector body) = case advance resolve selector of
  Done (Right value) -> Done value
  Done (Left value) -> advance resolve (Apply body (Pure value))
  Blocked demand rest -> Blocked demand (Select rest body)

data Facts = Facts
  (Map ProductId Products.ProductDetails)
  (Map UserId Users.UserDetails)

type Demand = (Set ProductId, Set UserId)

resolveRequest :: Facts -> Request a -> Either Demand a
resolveRequest (Facts products _) (ProductRequest identifier) =
  maybe (Left (Set.singleton identifier, Set.empty)) Right (Map.lookup identifier products)
resolveRequest (Facts _ users) (UserRequest identifier) =
  maybe (Left (Set.empty, Set.singleton identifier)) Right (Map.lookup identifier users)

runFetch
  :: MonadUnliftIO m
  => Products.ProductDetailsClient m
  -> Users.UserDetailsClient m
  -> Fetch Request a
  -> m a
runFetch products users = go (Facts Map.empty Map.empty)
  where
    go facts@(Facts knownProducts knownUsers) program =
      case advance (resolveRequest facts) program of
        Done value -> pure value
        Blocked (productIds, userIds) rest -> do
          (newProducts, newUsers) <- concurrently
            (fetchRequired "product" (Products.fetchBatch products) productIds)
            (fetchRequired "user" (Users.fetchBatch users) userIds)
          go (Facts (Map.union knownProducts newProducts) (Map.union knownUsers newUsers)) rest

fetchRequired
  :: (MonadUnliftIO m, Ord key, Show key)
  => String
  -> (NESet key -> m (Map key value))
  -> Set key
  -> m (Map key value)
fetchRequired source batch identifiers = case NESet.nonEmptySet identifiers of
  Nothing -> pure Map.empty
  Just requested -> do
    returned <- batch requested
    let facts = Map.restrictKeys returned identifiers
        missing = Set.difference identifiers (Map.keysSet facts)
    if Set.null missing
      then pure facts
      else throwIO (userError ("Missing " ++ source ++ " details: " ++ show (Set.toList missing)))
