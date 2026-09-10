{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}

module Incentives.EligibilityExpr where

import Data.Functor.Foldable (Base, Corecursive (..), Recursive (..))

data EligibilityExpr rule
  = Check rule
  | And (EligibilityExpr rule) (EligibilityExpr rule)
  | Or (EligibilityExpr rule) (EligibilityExpr rule)
  | Not (EligibilityExpr rule)
  | AnyLine (EligibilityExpr rule)
  | EveryLine (EligibilityExpr rule)
  deriving (Eq, Show, Functor, Foldable, Traversable)

data EligibilityExprF rule r
  = CheckF rule
  | AndF r r
  | OrF r r
  | NotF r
  | AnyLineF r
  | EveryLineF r
  deriving (Eq, Show, Functor, Foldable, Traversable)

type instance Base (EligibilityExpr rule) = EligibilityExprF rule

instance Recursive (EligibilityExpr rule) where
  project (Check rule) = CheckF rule
  project (And left right) = AndF left right
  project (Or left right) = OrF left right
  project (Not child) = NotF child
  project (AnyLine child) = AnyLineF child
  project (EveryLine child) = EveryLineF child

instance Corecursive (EligibilityExpr rule) where
  embed (CheckF rule) = Check rule
  embed (AndF left right) = And left right
  embed (OrF left right) = Or left right
  embed (NotF child) = Not child
  embed (AnyLineF child) = AnyLine child
  embed (EveryLineF child) = EveryLine child

infixr 3 .&&.
(.&&.) :: EligibilityExpr rule -> EligibilityExpr rule -> EligibilityExpr rule
(.&&.) = And

infixr 2 .||.
(.||.) :: EligibilityExpr rule -> EligibilityExpr rule -> EligibilityExpr rule
(.||.) = Or
