module Incentives.Client.UserDetails where

import Data.Map.Strict (Map)
import Data.Set.NonEmpty (NESet)
import Data.Time (UTCTime)
import Incentives.CheckoutSummary (UserId)

data UserDetails = UserDetails
  { userId :: UserId
  , accountCreatedAt :: UTCTime
  }
  deriving (Eq, Show)

newtype UserDetailsClient m = UserDetailsClient
  { fetchBatch :: NESet UserId -> m (Map UserId UserDetails)
  }
