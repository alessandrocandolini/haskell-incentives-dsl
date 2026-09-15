module Incentives.Client.ProductDetails where

import Data.Map.Strict (Map)
import Data.Set.NonEmpty (NESet)
import Data.Text (Text)
import Incentives.CheckoutSummary (ProductId)

data ProductDetails = ProductDetails
  { productId :: ProductId
  , category :: Text
  }
  deriving (Eq, Show)

newtype ProductDetailsClient m = ProductDetailsClient
  { fetchBatch :: NESet ProductId -> m (Map ProductId ProductDetails)
  }
