{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

module Incentives.Match where

import Incentives.Eligibility (Eligibility (..))

data Match reason = Hit reason | Miss reason
  deriving (Eq, Show, Functor, Foldable, Traversable)

eligibilityOf :: Match reason -> Eligibility
eligibilityOf (Hit _) = Eligible
eligibilityOf (Miss _) = NotEligible

match :: Bool -> reason -> Match reason
match True = Hit
match False = Miss
