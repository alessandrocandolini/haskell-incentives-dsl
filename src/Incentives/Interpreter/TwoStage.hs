{-# LANGUAGE DeriveFunctor #-}

module Incentives.Interpreter.TwoStage
  ( Staged (..)
  , known
  , deferred
  , interpretAstTwoStage
  ) where

import Data.Functor.Identity (Identity, runIdentity)
import Incentives.Ast (Ast)
import Incentives.Eligibility (Eligibility)
import Incentives.Interpreter (Interpreter (..))
import Incentives.Interpreter.Ast (interpretAst, minimumPartialWitness, minimumWitness)

-- The first pass produces values or pending actions; it cannot run the actions.
data Staged m a = Known a | Deferred (m a)
  deriving (Functor)

known :: Interpreter Identity context input output -> Interpreter Identity context input (Staged m output)
known = fmap Known

deferred :: Interpreter m context input output -> Interpreter Identity context input (Staged m output)
deferred interpreter = Interpreter $ \context input ->
  pure (Deferred (evaluate interpreter context input))

interpretAstTwoStage
  :: Applicative m
  => (result -> Eligibility)
  -> Interpreter Identity context rule (Ast (Staged m result))
  -> Interpreter m context (Ast rule) (Ast result)
interpretAstTwoStage verdict firstPass = Interpreter $ \context expression ->
  let partial = runIdentity (evaluate (interpretAst firstPass) context expression)
      remaining = minimumPartialWitness partialVerdict partial
  in minimumWitness verdict <$> traverse resolve remaining
  where
    partialVerdict (Known value) = Just (verdict value)
    partialVerdict (Deferred _) = Nothing
    resolve (Known value) = pure value
    resolve (Deferred action) = action
