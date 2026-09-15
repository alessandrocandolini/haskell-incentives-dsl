module Incentives.Interpreter.Ast (interpretAst, minimumWitness, minimumPartialWitness) where

import Control.Monad (join)
import Data.Functor.Foldable (cata)
import Incentives.Ast (Ast (..), AstF (..))
import Incentives.Eligibility (Eligibility (..))
import qualified Incentives.Eligibility as Eligibility
import Incentives.Interpreter (Interpreter (..))

-- Full interpretation: evaluate every leaf and substitute its resulting tree.
-- A leaf may expand, for example when a quantifier ranges over checkout lines.
interpretAst
  :: Applicative m
  => Interpreter m context rule (Ast result)
  -> Interpreter m context (Ast rule) (Ast result)
interpretAst interpreter = Interpreter $ \context expression ->
  join <$> traverse (evaluate interpreter context) expression

-- Choose a sufficient Boolean witness with the fewest leaves; ties prefer the
-- left branch. This is structural pruning of known results, not request pruning
-- or a search for logical relationships between different predicates.
minimumWitness :: (a -> Eligibility) -> Ast a -> Ast a
minimumWitness verdict = minimumPartialWitness (Just . verdict)

-- Keep unresolved branches unless a known sibling settles their result. Known
-- evidence stays in the tree, so resolving the remainder preserves explanations.
minimumPartialWitness :: (a -> Maybe Eligibility) -> Ast a -> Ast a
minimumPartialWitness verdict = tree . cata algebra
  where
    tree (_, _, expression) = expression
    size (_, count, _) = count
    result (eligibility, _, _) = eligibility
    smaller left right = if size left <= size right then left else right
    combine constructor eligibility left right =
      (eligibility, size left + size right, constructor (tree left) (tree right))
    algebra (PureF value) = (verdict value, 1 :: Int, Pure value)
    algebra (AndF left right) = case (result left, result right) of
      (Just NotEligible, Just NotEligible) -> smaller left right
      (Just NotEligible, _) -> left
      (_, Just NotEligible) -> right
      _ -> combine And
        (Eligibility.partialEligibilityAlgebra (AndF (result left) (result right))) left right
    algebra (OrF left right) = case (result left, result right) of
      (Just Eligible, Just Eligible) -> smaller left right
      (Just Eligible, _) -> left
      (_, Just Eligible) -> right
      _ -> combine Or
        (Eligibility.partialEligibilityAlgebra (OrF (result left) (result right))) left right
    algebra (NotF child) =
      (Eligibility.partialEligibilityAlgebra (NotF (result child)), size child, Not (tree child))
