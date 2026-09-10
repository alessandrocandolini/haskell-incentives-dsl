module Incentives.Client.UserDetails where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set.NonEmpty (NESet)
import qualified Data.Set.NonEmpty as NESet
import Data.Time (UTCTime)
import Incentives.CheckoutSummary (UserId)

data UserDetails = UserDetails
  { userId :: UserId
  , accountCreatedAt :: UTCTime
  }
  deriving (Eq, Show)

data UserDetailsClient m = UserDetailsClient
  { -- | Fetch one user; unknown IDs yield Nothing.
    fetch :: UserId -> m (Maybe UserDetails)
    -- | Fetch a non-empty set of IDs. Return only requested, known users;
    -- missing IDs are omitted, so the result may be empty.
  , fetchBatch :: NESet UserId -> m (Map UserId UserDetails)
  }

-- | Derive single lookup from the same batch operation, preserving its effects.
fromFetchBatch
  :: Functor m
  => (NESet UserId -> m (Map UserId UserDetails))
  -> UserDetailsClient m
fromFetchBatch batch = UserDetailsClient
  { fetch = \identifier -> Map.lookup identifier <$> batch (NESet.singleton identifier)
  , fetchBatch = batch
  }
