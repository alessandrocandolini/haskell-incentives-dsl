module Incentives.Client.ProductDetails where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set.NonEmpty (NESet)
import qualified Data.Set.NonEmpty as NESet
import Data.Text (Text)
import Incentives.CheckoutSummary (ProductId)

data ProductDetails = ProductDetails
  { productId :: ProductId
  , category :: Text
  }
  deriving (Eq, Show)

data ProductDetailsClient m = ProductDetailsClient
  { -- | Fetch one product; unknown IDs yield Nothing.
    fetch :: ProductId -> m (Maybe ProductDetails)
    -- | Fetch a non-empty set of IDs. Return only requested, known products;
    -- missing IDs are omitted, so the result may be empty.
  , fetchBatch :: NESet ProductId -> m (Map ProductId ProductDetails)
  }

-- | Derive single lookup from the same batch operation, preserving its effects.
fromFetchBatch
  :: Functor m
  => (NESet ProductId -> m (Map ProductId ProductDetails))
  -> ProductDetailsClient m
fromFetchBatch batch = ProductDetailsClient
  { fetch = \identifier -> Map.lookup identifier <$> batch (NESet.singleton identifier)
  , fetchBatch = batch
  }
