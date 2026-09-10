module Incentives.Client.Stub where

import qualified Data.Map.Strict as Map
import qualified Data.Set.NonEmpty as NESet
import qualified Incentives.Client.ProductDetails as ProductDetails
import qualified Incentives.Client.UserDetails as UserDetails
import Incentives.ExampleData (productDetailsById, userDetailsById)

stubProductDetailsClient :: Applicative m => ProductDetails.ProductDetailsClient m
stubProductDetailsClient = ProductDetails.fromFetchBatch $ \identifiers ->
  pure (Map.restrictKeys productDetailsById (NESet.toSet identifiers))

stubUserDetailsClient :: Applicative m => UserDetails.UserDetailsClient m
stubUserDetailsClient = UserDetails.fromFetchBatch $ \identifiers ->
  pure (Map.restrictKeys userDetailsById (NESet.toSet identifiers))
