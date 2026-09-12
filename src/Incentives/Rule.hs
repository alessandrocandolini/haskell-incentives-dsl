{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

module Incentives.Rule where

import Data.Text (Text)
import Numeric.Natural (Natural)
import Data.Tree (Tree (..), drawTree)
import Incentives.Ast (Ast (..))
import Incentives.CheckoutSummary (Currency, Days, Price, ShippingProvider)

data Target = Line | Purchase

data LineRule
  = PriceLessThan Price
  | ProductCategoryIs Text
  | SellerAccountAgeGreaterThan Days
  deriving (Eq, Show)

data PurchaseRule
  = CurrencyIs Currency
  | ShippingProviderIs ShippingProvider
  | BuyerAccountAgeGreaterThan Days
  | DistinctSellerCountGreaterThan Natural
  deriving (Eq, Show)

-- The index describes available context. Purchase facts are also available
-- while evaluating a line; line facts require a current line.
data Rule (context :: Target) where
  LineRule :: LineRule -> Rule 'Line
  PurchaseRule :: PurchaseRule -> Rule context
  AnyLine :: Ast (Rule 'Line) -> Rule context
  EveryLine :: Ast (Rule 'Line) -> Rule context

deriving instance Eq (Rule context)
deriving instance Show (Rule context)

line :: LineRule -> Ast (Rule 'Line)
line = Pure . LineRule

purchase :: PurchaseRule -> Ast (Rule context)
purchase = Pure . PurchaseRule

-- Quantifiers introduce a line for their body. The resulting purchase fact
-- is available in either context, including inside another quantifier.
anyLine :: Ast (Rule 'Line) -> Ast (Rule context)
anyLine = Pure . AnyLine

everyLine :: Ast (Rule 'Line) -> Ast (Rule context)
everyLine = Pure . EveryLine

-- This abbreviation renders as its primitive definition.
isBundle :: Ast (Rule context)
isBundle = purchase (DistinctSellerCountGreaterThan 1)

-- Unlike generic prettyAst, this also opens the ASTs inside quantifiers.
prettyRules :: Ast (Rule context) -> String
prettyRules = drawTree . ruleTree

ruleTree :: Ast (Rule context) -> Tree String
ruleTree expression = case expression of
  Pure (AnyLine child) -> Node "AnyLine" [ruleTree child]
  Pure (EveryLine child) -> Node "EveryLine" [ruleTree child]
  Pure rule -> Node (show rule) []
  And left right -> Node "And" [ruleTree left, ruleTree right]
  Or left right -> Node "Or" [ruleTree left, ruleTree right]
  Not child -> Node "Not" [ruleTree child]
