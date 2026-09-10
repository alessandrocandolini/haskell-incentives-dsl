module Incentives.CheckoutSummary where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Numeric.Natural (Natural)

newtype ProductId = ProductId Text deriving (Eq, Ord, Show)

newtype UserId = UserId Text deriving (Eq, Ord, Show)

data Currency = USD | GBP | EUR deriving (Eq, Ord, Show)

newtype Days = Days Natural deriving (Eq, Ord, Show)

days :: Natural -> Days
days = Days

-- Exact amounts in the checkout currency's major units.
newtype Price = Price Rational deriving (Eq, Ord, Show)

amount :: Rational -> Price
amount = Price

data ShippingProvider
  = USPS
  | UPS
  | FedEx
  | DHL
  | AmazonLogistics
  | RoyalMail
  | Evri
  | DPD
  | Yodel
  deriving (Eq, Ord, Show, Enum, Bounded)

data ShippingSummary = ShippingSummary
  { shippingProvider :: ShippingProvider
  , shippingPrice :: Price
  }
  deriving (Eq, Show)

data Line = Line
  { productId :: ProductId
  , sellerId :: UserId
  , currentPrice :: Price
  , originalPrice :: Price
  }
  deriving (Eq, Show)

data CheckoutSummary = CheckoutSummary
  { currency :: Currency
  , buyerId :: UserId
  , shipping :: ShippingSummary
  , lines :: NonEmpty Line
  }
  deriving (Eq, Show)
