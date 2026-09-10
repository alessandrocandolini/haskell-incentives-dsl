{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}

module Incentives.Evaluation where

import Data.Functor.Foldable (Base, Corecursive (..), Recursive (..), cata)
import Data.List.NonEmpty (NonEmpty)
import qualified Incentives.Ast as Ast
import Incentives.CheckoutSummary (LineId)
import Incentives.Eligibility (Eligibility (..), eligibilityAlgebra, fromBool)

data Evaluation observed rule
  = Checked observed rule Eligibility
  | EvaluatedAnd (Evaluation observed rule) (Evaluation observed rule)
  | EvaluatedOr (Evaluation observed rule) (Evaluation observed rule)
  | EvaluatedNot (Evaluation observed rule)
  | EvaluatedAnyLine (NonEmpty (LineId, Evaluation observed rule))
  | EvaluatedEveryLine (NonEmpty (LineId, Evaluation observed rule))
  deriving (Eq, Show, Functor, Foldable, Traversable)

data EvaluationF observed rule r
  = CheckedF observed rule Eligibility
  | EvaluatedAndF r r
  | EvaluatedOrF r r
  | EvaluatedNotF r
  | EvaluatedAnyLineF (NonEmpty (LineId, r))
  | EvaluatedEveryLineF (NonEmpty (LineId, r))
  deriving (Eq, Show, Functor, Foldable, Traversable)

type instance Base (Evaluation observed rule) = EvaluationF observed rule

instance Recursive (Evaluation observed rule) where
  project (Checked observed rule verdict) = CheckedF observed rule verdict
  project (EvaluatedAnd left right) = EvaluatedAndF left right
  project (EvaluatedOr left right) = EvaluatedOrF left right
  project (EvaluatedNot child) = EvaluatedNotF child
  project (EvaluatedAnyLine children) = EvaluatedAnyLineF children
  project (EvaluatedEveryLine children) = EvaluatedEveryLineF children

instance Corecursive (Evaluation observed rule) where
  embed (CheckedF observed rule verdict) = Checked observed rule verdict
  embed (EvaluatedAndF left right) = EvaluatedAnd left right
  embed (EvaluatedOrF left right) = EvaluatedOr left right
  embed (EvaluatedNotF child) = EvaluatedNot child
  embed (EvaluatedAnyLineF children) = EvaluatedAnyLine children
  embed (EvaluatedEveryLineF children) = EvaluatedEveryLine children

collapse :: Evaluation observed rule -> Eligibility
collapse = cata algebra
  where
    algebra (CheckedF _ _ verdict) = verdict
    algebra (EvaluatedAndF left right) = eligibilityAlgebra (Ast.AndF left right)
    algebra (EvaluatedOrF left right) = eligibilityAlgebra (Ast.OrF left right)
    algebra (EvaluatedNotF child) = eligibilityAlgebra (Ast.NotF child)
    algebra (EvaluatedAnyLineF children) = fromBool (any ((== Eligible) . snd) children)
    algebra (EvaluatedEveryLineF children) = fromBool (all ((== Eligible) . snd) children)
