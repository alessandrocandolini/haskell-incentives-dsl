module Incentives.ExampleData where

import Prelude hiding (lines)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Time (UTCTime (..), addUTCTime, fromGregorian)
import Incentives.CheckoutSummary
import qualified Incentives.Client.ProductDetails as ProductDetails
import qualified Incentives.Client.UserDetails as UserDetails

cheapPrice, originalBookPrice, expensivePrice, priceThreshold, shippingCost :: Price
cheapPrice = amount 8
originalBookPrice = amount 12
expensivePrice = amount 20
priceThreshold = amount 10
shippingCost = amount 5

bookId, electronicsId, clothingId :: ProductId
bookId = ProductId "1234"
electronicsId = ProductId "5678"
clothingId = ProductId "9012"

exampleBuyerId, establishedSellerId, recentSellerId :: UserId
exampleBuyerId = UserId "buyer-1"
establishedSellerId = UserId "seller-1"
recentSellerId = UserId "seller-2"

establishedAccountAge, recentAccountAge :: Days
establishedAccountAge = days 730
recentAccountAge = days 10

exampleNow, campaignStartsAt, campaignEndsAt :: UTCTime
exampleNow = UTCTime (fromGregorian 2026 9 10) 0
campaignStartsAt = UTCTime (fromGregorian 2026 9 1) 0
campaignEndsAt = UTCTime (fromGregorian 2026 10 1) 0

bookLine, electronicsLine, clothingLine :: Line
bookLine = Line bookId establishedSellerId cheapPrice originalBookPrice
electronicsLine = Line electronicsId establishedSellerId expensivePrice expensivePrice
clothingLine = Line clothingId recentSellerId cheapPrice cheapPrice

exampleShipping :: ShippingSummary
exampleShipping = ShippingSummary USPS shippingCost

exampleCheckout :: CheckoutSummary
exampleCheckout = CheckoutSummary
  { currency = USD
  , buyerId = exampleBuyerId
  , shipping = exampleShipping
  , lines = bookLine :| [electronicsLine, clothingLine]
  }

bookDetails, electronicsDetails, clothingDetails :: ProductDetails.ProductDetails
bookDetails = ProductDetails.ProductDetails bookId "books"
electronicsDetails = ProductDetails.ProductDetails electronicsId "electronics"
clothingDetails = ProductDetails.ProductDetails clothingId "clothing"

productDetailsById :: Map ProductId ProductDetails.ProductDetails
productDetailsById = Map.fromList
  [ (bookId, bookDetails)
  , (electronicsId, electronicsDetails)
  , (clothingId, clothingDetails)
  ]

buyerDetails, establishedSellerDetails, recentSellerDetails :: UserDetails.UserDetails
buyerDetails = UserDetails.UserDetails exampleBuyerId (createdAt establishedAccountAge)
establishedSellerDetails = UserDetails.UserDetails establishedSellerId (createdAt establishedAccountAge)
recentSellerDetails = UserDetails.UserDetails recentSellerId (createdAt recentAccountAge)

userDetailsById :: Map UserId UserDetails.UserDetails
userDetailsById = Map.fromList
  [ (exampleBuyerId, buyerDetails)
  , (establishedSellerId, establishedSellerDetails)
  , (recentSellerId, recentSellerDetails)
  ]

createdAt :: Days -> UTCTime
createdAt (Days age) = addUTCTime (negate (fromIntegral age * 86400)) exampleNow
