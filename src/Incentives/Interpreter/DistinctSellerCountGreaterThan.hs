module Incentives.Interpreter.DistinctSellerCountGreaterThan where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import Incentives.CheckoutSummary (UserId)
import Incentives.Eligibility (Eligibility, fromBool)
import Incentives.Interpreter (Interpreter (..))

-- | Context is the seller ID of each checkout line; input is the exclusive bound.
distinctSellerCountGreaterThan
  :: Applicative m
  => Interpreter m (NonEmpty UserId) Natural Eligibility
distinctSellerCountGreaterThan = Interpreter $ \sellerIds threshold ->
  let distinctCount = fromIntegral (Set.size (Set.fromList (NonEmpty.toList sellerIds)))
  in pure (fromBool (distinctCount > threshold))
