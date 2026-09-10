module Incentives.Client.ProductDetails where

import Data.Text (Text)
import Incentives.CheckoutSummary (ProductId)

data ProductDetails = ProductDetails
  { productId :: ProductId
  , category :: Text
  }
  deriving (Eq, Show)

newtype ProductDetailsClient m = ProductDetailsClient
  { fetchProductDetails :: ProductId -> m ProductDetails
  }
