module Incentives.Interpreter.Ast (interpretAst) where

import Incentives.Ast (Ast)
import Incentives.Eligibility (Eligibility)
import qualified Incentives.Eligibility as Eligibility
import Incentives.Interpreter (Interpreter (..))

-- Full, unoptimised interpretation: perform the leaf effects, then fold.
interpretAst
  :: Applicative m
  => Interpreter m context rule Eligibility
  -> Interpreter m context (Ast rule) Eligibility
interpretAst interpreter = Interpreter $ \context expression ->
  Eligibility.evaluate <$> traverse (evaluate interpreter context) expression
