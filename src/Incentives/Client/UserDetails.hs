module Incentives.Client.UserDetails where

import Data.Time (UTCTime)
import Incentives.CheckoutSummary (UserId)

data UserDetails = UserDetails
  { userId :: UserId
  , accountCreatedAt :: UTCTime
  }
  deriving (Eq, Show)

newtype UserDetailsClient m = UserDetailsClient
  { fetchUserDetails :: UserId -> m UserDetails
  }
