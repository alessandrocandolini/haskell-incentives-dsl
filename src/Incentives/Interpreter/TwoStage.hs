module Incentives.Interpreter.TwoStage
  ( interpretAstTwoStage
  ) where

import Data.Functor.Identity (Identity, runIdentity)
import Incentives.Ast (Ast (..))
import Incentives.Eligibility (Eligibility)
import Incentives.Interpreter (Interpreter (..))
import Incentives.Interpreter.Ast (interpretAst, minimumPartialWitness, minimumWitness)

interpretAstTwoStage
  :: Applicative m
  => (result -> Eligibility)
  -> Interpreter Identity context rule (Ast (Either result remaining))
  -> Interpreter m context remaining (Ast result)
  -> Interpreter m context (Ast rule) (Ast result)
interpretAstTwoStage verdict firstPass secondPass = Interpreter $ \context expression ->
  let partial = runIdentity (evaluate (interpretAst firstPass) context expression)
      remaining = minimumPartialWitness (either (Just . verdict) (const Nothing)) partial
      resolve = Interpreter $ \current -> either (pure . Pure) (evaluate secondPass current)
  in minimumWitness verdict <$> evaluate (interpretAst resolve) context remaining
